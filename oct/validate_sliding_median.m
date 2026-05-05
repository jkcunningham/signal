function [] = validate_sliding_median(fs,win,frame_k,src_csv,med_csv)
  P1 = csvread(src_csv);        # PSD spectral amplitudes (power spectra)
  size(P1)                      # 108 x 2048 for 10 seconds at 44100 Hza
  N = size(P1,2);               # N is the size of the positive half-spectrum 
  df = fs / (2*N);              # frequency increment
  f = (0:N-1) * df;             # frequency vector for x-axes

  ## P1(P1<=0) = 1.e-12;           # protect logs from underflow 
  ## P1db = 10*log10(P1);

  Pv = movmed(P1, win, 0, 2);    # apply filter to power
  Pvk = Pv(frame_k,:);
  Pvkdb = 10*log10(Pvk);

  lPv = csvread(med_csv);      # output of Lisp sliding median filter on P1
  lPvk = lPv(frame_k,:);
  lPvkdb = 10*log10(lPvk);

  plot(f,Pvkdb,'r');            # red underneath so it shows
  hold on
  plot(f,lPvkdb,'k');
  hold off
  axis([0 fs/2]); grid on; 
endfunction

