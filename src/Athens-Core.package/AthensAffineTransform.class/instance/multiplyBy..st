transformations
multiplyBy: m

	"multiply receiver by given affine matrix"
"
| sx   shx   x   |        | sx'   shx'   x'   | 
| shy sy     y    |   *    | shy' sy'     y'   |
|  0     0     1   |        | 0  	0   		1	|       

"	
	
	| nsx nshx nx nshy nsy ny |
	
	nsx := sx * m sx + (shx  * m shy).
	nshx := sx * m shx + (shx * m sy).
	nx := sx * m x + (shx * m y) + x.
	
	nshy := shy * m sx + (sy * m shy).
	nsy := shy * m shx  + (sy * m sy).
	ny := shy* m x + (sy * m y) + y.
	
	sx := nsx.
	sy := nsy.
	shx := nshx.
	shy := nshy.
	x := nx.
	y := ny.		