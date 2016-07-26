class initialization
initialize
	"Float initialize"
	"Constants from Computer Approximations, pp. 182-183:
		Pi = 3.14159265358979323846264338327950288
		Pi/2 = 1.57079632679489661923132169163975144
		Pi*2 = 6.28318530717958647692528676655900576
		Pi/180 = 0.01745329251994329576923690768488612
		2.0 ln = 0.69314718055994530941723212145817657
		2.0 sqrt = 1.41421356237309504880168872420969808"

	Pi := 3.14159265358979323846264338327950288.
	Halfpi := Pi / 2.0.
	Twopi := Pi * 2.0.
	ThreePi := Pi * 3.0.
	RadiansPerDegree := Pi / 180.0.

	Ln2 := 0.69314718055994530941723212145817657.
	Ln10 := 10.0 ln.
	Sqrt2 := 1.41421356237309504880168872420969808.
	E := 2.718281828459045235360287471353.

	Epsilon := 0.000000000001.  "Defines precision of mathematical functions"

	MaxVal := 1.7976931348623157e308.
	MaxValLn := 709.782712893384.
	MinValLogBase2 := -1074.

	Infinity := MaxVal * MaxVal.
	NegativeInfinity := 0.0 - Infinity.
	NaN := Infinity - Infinity.
	NegativeZero := 1.0 / Infinity negated.
