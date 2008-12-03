library(VICCBiostat)

# 0
info.test(rep(0,5), rep(1,5))
# 0.9090909
info.test(2*(1:5), 2*(1:6)-1)
# 0.9950249
info.test(2*(1:100), 2*(1:101)-1)
