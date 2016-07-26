transformations
scaleBy: factor

	"multiply receiver by uniform scale matrix

| sx   shx   x   |       | (f x) 0 0  |             | sx*(f x)    shx*(f y)  x |
| shy sy     y   |   *   | 0 (f y) 0  |    ===> | shy*(f x)     sy*(f y)    y |
|  0     0     1  |        | 0 0     1  |                |  0      0              1  |

"	
	factor isPoint ifTrue: [
		sx := sx*factor x.
		shx := shx*factor y.
		sy := sy*factor y.
		shy := shy*factor x. 		
	] ifFalse: [
		sx := sx*factor.
		shx := shx*factor.
		sy := sy*factor.
		shy := shy*factor. ]
