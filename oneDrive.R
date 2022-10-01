# oneDrive.R

f <- "https://je9ypg.bn.files.1drv.com/y4mp70jUIzFoVG0yL_qpkidARHXsBcYsTW1jGkOPedso8_5yw4wJiUVTpDsCRrAXtzNBXNxdp_4F7I5vA4Mq8D029t3a5XFQ-IAT8zHwTgJ20hHQTX744l0yKs8y5n_uQ8yRYuwrxSccr7j8RYAun3rLHpYAyyPcDt2jWYm7ph48EW-Sa-vA4zKQKhuU2Z_zJ0_/DAILY%20ATTENDANCE%20OCT%202019.xlsx"
dest.name <- "data/10_19.xlsx"

download.file(url = f,destfile=dest.name,mode = "w")
