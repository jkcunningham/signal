function [db] = todb(x, minval=1.0e-12)

  x(x<=0) = minval;
  db = 10*log10(x);

endfunction
