Class Number holds the most general methods for dealing with numbers. Subclasses Float, Fraction, and Integer, and their subclasses, provide concrete representations of a numeric quantity.

All of Number's subclasses participate in a simple type coercion mechanism that supports mixed-mode arithmetic and comparisons.  It works as follows:  If
	self<typeA> op: arg<typeB>
fails because of incompatible types, then it is retried in the following guise:
	(arg adaptTypeA: self) op: arg adaptToTypeA.
This gives the arg of typeB an opportunity to resolve the incompatibility, knowing exactly what two types are involved.  If self is more general, then arg will be converted, and viceVersa.  This mechanism is extensible to any new number classes that one might wish to add to Pharo.  The only requirement is that every subclass of Number must support a pair of conversion methods specific to each of the other subclasses of Number.

Implementation notes
----------------------------------
The implementation of #degreeCos and #degreeSin is such that results are exact for any multiple of 90.

Care is also taken to evaluate the sine between -90° and 90°, this will avoid #degreesToRadians and i386 FPU sine function to accumulate round off errors due to approximate representation of pi.
We can thus evaluate 240 degreeCos with at most 1 ulp error. It's not perfect, but better than previous implementation.

For cosine, we know that:
	cosd(x)=cosd(abs(x))
	cosd(x)=sind(90-x)
thus the trick is to evaluate:
	cosd(x)=sind(90-abs(x)) after appropriate modulo in [-180,180[
This way, we are sure to evaluate the sine between -90° and 90°
The #degreesToRadians and #sin are used rather than #degreeSin to avoid cycles.

For sine, it would be necessary to evaluate either
sind(x) if abs(x) <=90
or sind(180-x) if abs(x) >= 90
A possible implementation would be:
	| x |
	x := 90 + self \\ 360 - 90.
	x >= 180 ifTrue: [x := 180 - x].
	^x degreesToRadians sin
We prefer evaluating cosd(90-x) thus providing a branch free implementation.