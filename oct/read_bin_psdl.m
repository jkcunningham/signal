function [psdl,hdr] = read_bin_psdl (fn)
  ## Reads binary file FN and resturns a PSDL matrix and any header as values.
  fn,
  fid = fopen(fn, 'rb', 'ieee-le');

  ## Always present
  nhdr = fread(fid, [1,1], 'double');
  nfrm = fread(fid, [1,1], 'double'); # hdr(1)
  npsd = fread(fid, [1,1], 'double'); # hdr(2)

  ## Only present if nfrm > 0
  hdr = [];
  if (nhdr > 2)
    for i = 1:nhdr-2
      hdr = [hdr fread(fid, [1,1], 'double')];
    endfor
  endif

  ## Present if both nhdr>0 and nrfm > 0
  v = fread(fid, [npsd,nfrm], 'double')'; # octave reads psds into columns
  fclose(fid);

  ## Octave always reads column-major order
  psdl = reshape(v,nfrm,npsd);
  ## So I have to reshape it to get it right. 

endfunction
