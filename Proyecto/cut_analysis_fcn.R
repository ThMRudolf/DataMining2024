# limiting phi to 2*pi
lim22pi <- function(phi){
  N <- length(phi)
  id_reduce <- NULL
  for(k in 1:N){
    if(phi[k]>2*pi){
      phi[k:N] <- phi[k:N] -2*pi
      id_reduce <- append(id_reduce, k)
    }
  }
  
  listLim22pi <- list(phi, id_reduce)
  return(listLim22pi)
}

maf_rcpp <- function(in_signal, n) {
    # Moving average
    maf_signal <- roll_mean(in_signal, n, fill = NA, align = "right")
    
    # Moving standard deviation
    sdt_signal <- roll_sd(in_signal, n, fill = NA, align = "right")
    
    # Standardized signal
    mStdScOut <- (in_signal - maf_signal) / sdt_signal
    
    # Return as a data frame
    df <- data.frame(maf_signal, sdt_signal, mStdScOut)
    return(df)
}

# moving average filter /function moving scalar
maf <- function(in_signal, n){
  N <- length(in_signal)
  maf_signal <- rep(0, N)
  sdt_signal <- rep(0, N)
  mStdScOut <- rep(0, N)
  for(k1 in 1:n){
    maf_signal[k1] <- mean(in_signal[1:k1])
    sdt_signal[k1] <- sd(in_signal[1:k1])
    mStdScOut[k1] <- (in_signal[k1]-maf_signal[k1] )/sdt_signal[k1]
  }
  for(k2 in (n+1):N){
    maf_signal[k2] <- mean(in_signal[(k2-n+1):k2])
    sdt_signal[k2] <- sd(in_signal[(k2-n+1):k2])
    mStdScOut[k2] <- (in_signal[k2]-maf_signal[k2] )/sdt_signal[k2]
    
  }
  df = data.frame(maf_signal, sdt_signal, mStdScOut)
  return(df)
}

# find max Mc in 2pi
max_Mc <- function(id_reduce, Mc, phi){
  # find max in intevall of 2pi
  NMax <- length(id_reduce)
  idx <- 1
  k3_old <- 1
  McMax <- NULL
  phiMcMax <- NULL
  for(k3 in id_reduce){
    Mc_temp <- Mc[k3_old:k3]
    phi_temp <- phi[k3_old:k3]
    McMax <- append(McMax, max(Mc_temp))
    
    ii <- which(McMax[idx]==Mc_temp)
    #print(ii)
    phiMcMax <- append(phiMcMax, phi_temp[ii[1]])
    #print("angle:")
    #print( phi_temp[ii[1]])
    k3_old <- k3
    idx <- idx+1
  }
  df <- data.frame(McMax, phiMcMax)
  return(df)
}

# beta distribution alpha and beta by mean and varianz, echo = TRUE}
beta_dist_param <- function(m, var){
  alpha <- m^2*(1-m)/var - m
  beta <- alpha * (1-m)/m
  param <- data.frame(alpha, beta)
  return(param)
}



# function analyse specific amplitude}
get_amps_at_freq <- function(iq, freq_lim, print=FALSE)
{
  fft_iq <- fft(iq)
  nsamples = length(fft_iq )
  magnitude <- Mod(fft_iq[1:(nsamples/2)])
  
  freq <- seq(0, (nsamples/2 - 1)) * fs / nsamples
  
  top_indices = rep(0,2)
  # Identify the top 3 frequencies with the highest amplitudes
  index_rot = which(freq>freq_lim[1] & freq<freq_lim[2])
  index_cut = which(freq>freq_lim[3] & freq<freq_lim[4])
  top_indices[1] <- order(magnitude[index_rot], decreasing = TRUE)[1] +   index_rot[1] - 1 # Indices of the top 1 amplitudes
  top_indices[2] <- order(magnitude[index_cut], decreasing = TRUE)[1] + index_cut[1] - 1
  top_frequencies <- c(freq[top_indices[1]],freq[top_indices[2]])   # Corresponding     frequencies
  top_amplitudes <- magnitude[top_indices]  # Corresponding amplitudes
  # Display results
  if(print==TRUE){
    print(data.frame(Frequency = top_frequencies, Amplitude = top_amplitudes))
  }
  
  amp <- sum(top_amplitudes)*2/nsamples
  plotData = data.frame(freq, magnitude)
  
  amps_list <- list(amp = amp,
                    top_amplitudes = top_amplitudes,
                    top_frequencies = top_frequencies,
                    freq = freq, 
                    magnitude = magnitude)
  return(amps_list)
  
}
