function [sigma2] = dft_variance_collapse(fig, d, fs, ttl=false)
## Variance collapse detector
## d is DFTs across rows, time down columns
sigma2 = var(d);                # variance on each frequency over time. 
sigma2 = sigma2 / max(sigma2);  # normalize it
## sigma2 = sigma2 / min(sigma2);  # normalize it ** experiment
sigma2_db = 10*log10(sigma2);   # make it easier to see

if fig
  plot_fdomain(3, sigma2_db, fs, ttl, 'dB');
  ## Looking for first f where sigma2 < tau,
  ## where tau is 0.05-0.1 for 
  tau = 0.05;
  tau_dB = 10*log10(tau)
  hold on
  plot([0 fs/2],[tau_dB tau_dB],'r')
  hold off
end

## Result:
## Tau is way off from what it would take for this to find fc. It is almost
## entirely above the plot (fc ~ 1000 Hz).
## Seems like I should normalize by the minimum value.
## With tau = 2 dB this would give an fc ~ 16120

end
