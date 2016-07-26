transformations
scaleX: fx Y: fy

	"multiply receiver by scale matrix

| sx   shx   x   |        | fx 0 0  |              | sx*fx    shx*fx  x |
| shy sy     y   |   *   | 0 fy 0  |    ===> | shy*fy  sy*fy   y |
|  0     0     1   |       | 0 0  1  |              |  0          0        1  |

"
	sx := sx*fx.
	shx := shx*fx.
	sy := sy*fy.
	shy := shy*fy.
