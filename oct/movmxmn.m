function [y] = movmxmn(x,w)
  ## Run a sliding max-min filter on x

  y = movmax(x,w) - movmin(x,w);

end
