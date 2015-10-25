library(functional)
library(rgl)

valid <- function(block.coord) all(block.coord <=1) && all(block.coord >= -1)
only.valid <- function(block.list) block.list[sapply(block.list, valid)]

block.rotations <- function(block.coord) 
  mapply(`%*%`, unique.rotations, list(block.coord), SIMPLIFY=FALSE)

# All possible translations
translations <- as.matrix(expand.grid(rep(list((-2):2),3)))

block.shifts <- function(block.coord) 
  lapply(1:nrow(translations), function(x) block.coord + translations[x,])

shifts.and.rotations <- function(block.coord) {
  all.rotations <- block.rotations(block.coord)
  just.valid <- only.valid(unlist(lapply(all.rotations, block.shifts), recursive = FALSE))
  displayed <- lapply(just.valid, display)
  just.valid[!duplicated(displayed)]
}

# All possible rotation matrices.
x = matrix(c(1,0,0,0,cospi(1/2),sinpi(1/2),0,-sinpi(1/2),cospi(1/2)), nrow=3)
y = matrix(c(cospi(1/2),0,-sinpi(1/2),0,1,0,sinpi(1/2),0,cospi(1/2)), nrow=3)
z = matrix(c(cospi(1/2),sinpi(1/2),0,-sinpi(1/2),cospi(1/2),0,0,0,1), nrow=3)
l <- list(x,y,z, diag(3))

combos <- expand.grid(rep(list(seq_along(l)),3+1))
a <- unlist(apply(combos, 1, function(x) list(Reduce(`%*%`, l[x]))), recursive=FALSE)
unique.rotations <- unique(a)

# Blocks
green <- cbind(c(0,0,0), c(1,-1,0), c(1,0,0), c(1,1,0))
pink <- cbind(c(-1,-1,0), c(0,-1,0), c(0,0,0), c(1,0,0))
red <- cbind(c(-1,-1,0), c(-1,0,0),  c(0,0,0), c(1,0,0))
black <- cbind(c(-1,-1,0), c(-1,0,0),  c(0,0,0))
blue <- cbind(c(-1,-1,0), c(-1,0,0), c(-1,0,1),  c(0,0,0))
orange <- cbind(c(-1,-1,0), c(-1,0,0), c(-1,0,1),  c(0,0,1))
yellow <- cbind(c(-1,-1,1), c(-1,0,1), c(-1,0,0),  c(0,0,0))

blocks <- list(green=green, pink=pink, red=red, black=black, blue=blue, orange=orange, yellow=yellow)
block.colours <- names(blocks)

# Display blocks.
display <- function(block.coord) {
  block <- structure(rep(0, 3^3), .Dim = rep(3,3))
  block[t(block.coord + 2)] <- 1
  block
}

display.block <- function(block, colours) {
  block <- block * 2 # Unknown default scaling.
  invisible(mapply(function(x,y) 
    shade3d(translate3d( cube3d(col = y), x[1], x[2], x[3])), 
    data.frame(block), colours))
}

display.mat.block <- function(block) {
  a = apply(block[,grep("colour", colnames(block))] == 1, 2, which)
  col.order <- gsub('^colour', '', names(sort(a)))
  cols <- col.order[apply(block[,-grep("colour", colnames(block))] == 1, 2, which)]
  display.block(t(expand.grid(-1:1,-1:1,-1:1)), cols)
}

display.block.list <- function(block.list, colours){
  open3d()
  n <- ceiling(sqrt(length(block.list)))
  mfrow3d(n, n)
  for (i in block.list[1:(min(n^2, length(block.list)))]) {
    next3d()
    display.mat.block(i)
  }
}

# Start with a seed.
seed <- only.valid(block.shifts(blocks[[1]])) [1:4] # Fix later
left <- lapply(blocks[-1], shifts.and.rotations)
ready.for.matrix <- c(green=list(seed), left)

convert.to.mat.col <- function(x) c(display(x))
little.mats <- lapply(ready.for.matrix, function(x) t(sapply(x, convert.to.mat.col)))
colour.columns <- model.matrix(~colour-1,data.frame(colour=rep(names(ready.for.matrix), 
                                                               sapply(ready.for.matrix, length))))

big.mat <- cbind(do.call(rbind, little.mats), colour.columns)

solve.mat <- function(mat) {
  sums <- colSums(mat)
  if (all(sums==1)) return(list(mat))
  if (any(sums==0)) return(NULL)
  min.col <- match(min(sums[sums>1]), sums)
  rows <- which(mat[,min.col] == 1)
  reduce.by.row <- function(row) {
    piece.cols <- which(mat[row,]==1)
    elim.rows <- which(rowSums(mat[,piece.cols] == 1) > 0)
    mat[-setdiff(elim.rows, row),]
  }
  unlist(lapply(rows, Compose(reduce.by.row, solve.mat)), recursive=FALSE)
}

s<-solve.mat(big.mat)
display.block.list(s)
