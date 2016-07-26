transformations
rotateByRadians: angle

	"multiply receiver by rotation matrix

| sx   shx   x   |        | cos -sin 0  |             | (sx*cos)+(shx*sin)   (-sx*sin+shx*cos)  x |
| shy sy     y   |   *   | sin   cos 0 |   ===> | (shy*cos)+(sy*sin)   (-shy*sin)+sy*cos) y |
|  0     0     1   |       | 0     0   1   |             |        0                                 0                1  |

"
	| cos sin newSx newSy |
	
	cos := angle cos.
	sin := angle sin.
	
	
	newSx := sx*cos + (shx*sin).
	newSy := sy*cos - (shy*sin).
	
	shx := shx*cos - (sx*sin).
	shy := shy*cos + (sy*sin).

	sx := newSx.
	sy := newSy.