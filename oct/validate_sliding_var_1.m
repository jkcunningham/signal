function [d] = validate_sliding_var_1(med_bin,win,fig=false)
  [V2,fs] = read_bin_psdl (med_bin);
  Pi = V2(1,:);                 # input to Lisp filter
  Po = V2(2,:);                 # output from Lisp filter
  mPo = movvar(Pi, win, 0);     # apply filter 
  ## Compare
  err = Po - mPo;
  nerrs = sum(err != 0);
  mederr = median(abs(err));
  mxaerr = max(abs(err));
  fprintf('%i errs, max|errs|=%.3e, med(errs)=%.3e\n', nerrs, mxaerr, mederr);

  if false
    k = 233;
    badk = k-11:k+11;
    Pi(badk)',
    length(Pi(badk)),
    var_k = var(Pi(badk)),
    mpo_k = mPo(k),
    po_k  = Po(k),                # <--- Lisp is off by x 10^4 exactly on 233.
  end

  if fig
    figure(fig)
    subplot(2,1,1);
    plot(10*log10(Po),'r');            # red underneath so it shows
    hold on
    plot(10*log10(mPo),'k');

    hold off
    grid on; 
    title('Validate lisp sliding variance function against Octave');

    subplot(2,1,2);
    plot(err,'b');
  endif

  ## N = numel(Pi);                # N is the size of the positive half-spectrum
  ## df = fs / (2*N);              # frequency increment
  ## f = (0:N-1) * df;             # frequency vector for x-axes

  ## plot(f,Po,'r');            # red underneath so it shows
  ## hold on
  ## plot(f,mPo,'k');
  ## hold off
  ## axis([0 fs/2]); grid on; 

endfunction

