library(Gmisc)

# 0
info.test(rep(0:1,each=5))
# 0.9090909
info.test(2*(1:5), 2*(1:6)-1)
# 0.9950248
info.test(2*(1:100), 2*(1:101)-1)
