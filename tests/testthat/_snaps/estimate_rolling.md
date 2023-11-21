# `cfr_rolling`: Basic expectations

    Code
      head(rolling_scfr_naive, rows)
    Output
               date severity_mean severity_low severity_high
      1  1976-08-25    0.00000000   0.00000000     0.9750000
      2  1976-08-26    0.00000000   0.00000000     0.9750000
      3  1976-08-27    0.00000000   0.00000000     0.9750000
      4  1976-08-28    0.00000000   0.00000000     0.9750000
      5  1976-08-29    0.00000000   0.00000000     0.9750000
      6  1976-08-30    0.00000000   0.00000000     0.9750000
      7  1976-08-31    0.00000000   0.00000000     0.9750000
      8  1976-09-01    0.00000000   0.00000000     0.8418861
      9  1976-09-02    0.00000000   0.00000000     0.7075982
      10 1976-09-03    0.00000000   0.00000000     0.6023646
      11 1976-09-04    0.00000000   0.00000000     0.3694166
      12 1976-09-05    0.00000000   0.00000000     0.3362671
      13 1976-09-06    0.00000000   0.00000000     0.3084971
      14 1976-09-07    0.00000000   0.00000000     0.2470526
      15 1976-09-08    0.06666667   0.00168643     0.3194846

---

    Code
      head(rolling_scfr_corrected, rows)
    Output
               date severity_mean severity_low severity_high
      1  1976-08-25            NA           NA            NA
      2  1976-08-26         0.001        0.001         0.999
      3  1976-08-27         0.001        0.001         0.999
      4  1976-08-28         0.001        0.001         0.999
      5  1976-08-29         0.001        0.001         0.999
      6  1976-08-30         0.001        0.001         0.994
      7  1976-08-31         0.001        0.001         0.984
      8  1976-09-01         0.001        0.001         0.970
      9  1976-09-02         0.001        0.001         0.946
      10 1976-09-03         0.001        0.001         0.904
      11 1976-09-04         0.001        0.001         0.837
      12 1976-09-05         0.001        0.001         0.726
      13 1976-09-06         0.001        0.001         0.601
      14 1976-09-07         0.001        0.001         0.492
      15 1976-09-08         0.266        0.018         0.748
