## -*- texinfo -*-
## @deftypefn {} {@var{ed,cm} =} pipeline_1 (@var{x},@var{hw},@var{edge_sign},@var{x_min})
## Detects 1-d edges (steps) beyond X_MIN in X using a convolution window of width WC.
## 
## Inputs:@*
## ⬥ X a 1-d signal vector@*
## ⬥ X_MIN the minimum edge index (default 1)@*
## ⬥ HW is the half-width of the convolution step kernel@*
##     - if HW <= 0, the kernel is the full width of X, step at center@*
## ⬥ EDGE_SIGN is the sign of the convolution kernel (-1 for negative-edges)@*
##
## Returns:@*
## ➝ ED, a matrix with [index amplitude] rows for the edges, in decreasing order of amplitude.@*
## 
## ➝ CM is the output of the convolution (masked to start at x_min). 
## 
function [edges,cm] = edge_detector(x, wc, edge_sign=+1, x_min=1)
  N = numel(x);
  x = x - mean(x);
  x = x / norm(x);
  if wc
    ## Make a convolution kernel to match negative steps of width wfc Hz
    win = edge_sign * [-1*ones(1,wc),1*ones(1,wc)];
  else
    N = numel(sig);
    win = [-1*ones(1,N/2), ones(1,N/2)];
  endif
    win = win / norm(win);
    c = conv(x,win,'same');

  mask = x_min:N;               # exclude x below x_min from peak search
  xm = x(mask);
  cm = c(mask);

  ## ―――――――――――――――― maximum peak-finder
  if false
    ci = find(cm==max(cm));
    emax = ci + x_min - 1;
    ampl = cm(ci);
    edges = [emax ampl];
  endif

  ## ―――――――――――――――― multiple peak finder

  cmnn = cm - min(cm);       # findpeaks function doesn't allow negative numbers
  [pks, locs] = findpeaks(cmnn);

  fc2 = false; 
  if !isempty(pks)
    ## sort by magnitude
    [sorted, idx] = sort(pks);
    edges = [];
    for j=0:numel(sorted)-2
      ipk = idx(end-j);   
      this = [locs(ipk) sorted(end-j)];
      edges = [edges; this];
    endfor
  endif

endfunction

