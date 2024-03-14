#!/usr/bin/mawk -f

BEGIN { FS = ";"; }

{ k = $1; v = $2 + 0;

  if (!(k in min)) {
      min[k]=v; max[k]=v; sum[k]=v; count[k]=1;
  } else {
      if (v < min[k]) min[k] = v;
      if (v > max[k]) max[k] = v;
      sum[k] += v; count[k]++; } }

END { for (k in min) {
        avg = sum[k] / count[k];
        printf "%s;%.1f;%.1f;%.1f\n", k, min[k], max[k], avg; } }
