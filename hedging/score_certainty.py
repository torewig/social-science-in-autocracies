"""Score sentences with the Pei & Jurgens (EMNLP 2021) certainty model.

pedropei/sentence-level-certainty is SciBERT fine-tuned with a single
regression output on 2,167 annotated scientific findings, predicting certainty
on a 1-6 scale where higher is more certain. It is the learned counterpart to
the Hyland hedge list: the paper's own result is that hedge words explain only
part of what readers perceive as certainty, so this is a real test of whether
the lexicon measure was missing the construct.

Reads certainty_sentences.parquet, writes certainty_scores.parquet.
"""

import time
import numpy as np
import pandas as pd
import torch
from transformers import AutoTokenizer, AutoModelForSequenceClassification

MODEL = "pedropei/sentence-level-certainty"
IN_PATH = "/home/martigso/wos_parsed/hedging_probe/certainty_sentences.csv"
OUT_PATH = "/home/martigso/wos_parsed/hedging_probe/certainty_scores.csv"
BATCH_SIZE = 128
MAX_LENGTH = 256

device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
print("device:", device, flush=True)

tokenizer = AutoTokenizer.from_pretrained(MODEL)
model = AutoModelForSequenceClassification.from_pretrained(MODEL).to(device).eval()
print("model loaded, num_labels:", model.config.num_labels, flush=True)

frame = pd.read_csv(IN_PATH)
sentences = frame["sentence"].fillna("").astype(str).tolist()
print("sentences:", len(sentences), flush=True)

scores = np.empty(len(sentences), dtype=np.float32)
start = time.time()

with torch.inference_mode():
    for begin in range(0, len(sentences), BATCH_SIZE):
        batch = sentences[begin:begin + BATCH_SIZE]
        encoded = tokenizer(batch, padding=True, truncation=True,
                            max_length=MAX_LENGTH, return_tensors="pt")
        encoded = {k: v.to(device) for k, v in encoded.items()}
        logits = model(**encoded).logits
        scores[begin:begin + len(batch)] = logits[:, 0].float().cpu().numpy()
        if begin % (BATCH_SIZE * 200) == 0 and begin:
            done = begin + len(batch)
            rate = done / (time.time() - start)
            print(f"{done:,}/{len(sentences):,}  {rate:,.0f} sent/s  "
                  f"eta {(len(sentences) - done) / rate / 60:.1f} min", flush=True)

frame["certainty"] = scores
frame[["ut", "sent_id", "is_power_sentence", "certainty"]].to_csv(OUT_PATH, index=False)

print(f"\ndone in {(time.time() - start) / 60:.1f} min", flush=True)
print("certainty range: %.2f to %.2f, mean %.2f" %
      (scores.min(), scores.max(), scores.mean()), flush=True)
power = frame["is_power_sentence"] == 1
print("mean certainty, power sentences: %.3f" % frame.loc[power, "certainty"].mean(), flush=True)
print("mean certainty, other sentences: %.3f" % frame.loc[~power, "certainty"].mean(), flush=True)
