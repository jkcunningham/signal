function [] = plot_fdomain(fig, y, fs, ttl=false, ylbl=false)
  ## Assumes row(s) are frequency data

  N = size(y,2);                # N is the size of the positive half-spectrum 
  if (N == 1)
    N = size(y,1);              # in case y is a single column vector
  end
  df = fs / (2*N);                # frequency increment
  f = (0:N-1) * df;               # frequency vector for x-axes

  figure(fig);
  plot(f,y);
  grid
  xlabel('Frequency (Hz)');   % or whatever units apply

  if ylbl
    ylabel(ylbl);
  end

  if ttl
    title(ttl,'Interpreter','None','FontSize',14);
  end

  axis([0 fs/2]);
  grid on

end
