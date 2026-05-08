function [psdl,hdr] = read_binary_data (fn)
  ## Reads a matrix from a binary file written in a simple format.
  ## Numbers are all ieee-le double-floats.
  ## It begins with a simple linear header vector followed by the data matrix.
  ## Specifically,
  ## NHDR : 2 + number of header values
  ## NROW : number of rows in the data matrix
  ## NCOL : number of cols in the data matrix
  ## HVEC : vector of nhdr-2 values
  ## DATA : ncol x nrow matrix.

  warning('off', 'Octave:data-file-in-path');

  fid = fopen(fn, 'rb', 'ieee-le');

  ## Always present
  nhdr = fread(fid, [1,1], 'double');
  nrow = fread(fid, [1,1], 'double'); 
  ncol = fread(fid, [1,1], 'double'); 
  ## Only present if nrow > 0
  hdr = [];
  if (nhdr > 2)

    for i = 1:nhdr-2
      hdr = [hdr fread(fid, [1,1], 'double')];
    endfor

    ## Present if both nrow>0 and ncol > 0
    v = fread(fid, [ncol,nrow], 'double')'; # octave reads psds into columns
    fclose(fid);

    ## Octave reads column-major requiring reshape to make it so.
    psdl = reshape(v,nrow,ncol);

  endif

endfunction
