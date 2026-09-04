

sum(
  c(
    dbinom(3, 3, 0.25),
    (dbinom(2, 3, 0.25) * sum(dbinom(2:6, 6, 0.25))),
    (dbinom(1, 3, 0.25) * sum(dbinom(3:6, 6, 0.25)))
    
  )
)

sum(
  c(
    dbinom(3, 3, 0.5),
    (dbinom(2, 3, 0.5) * sum(dbinom(2:6, 6, 0.5))),
    (dbinom(1, 3, 0.5) * sum(dbinom(3:6, 6, 0.5)))
    
  )
)

3 * (dbinom(0, 3, 0.25) + dbinom(3, 3, 0.25)) + 
  9 * (sum(dbinom(1:2, 3, 0.25)))

3 * (dbinom(0, 3, 0.5) + dbinom(3, 3, 0.5)) + 
  9 * (sum(dbinom(1:2, 3, 0.5)))
