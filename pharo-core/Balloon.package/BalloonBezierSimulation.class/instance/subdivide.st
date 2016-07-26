computing
subdivide
	"Subdivide the receiver"
	| dy dx |
	"Test 1: If the bezier curve is not monoton in Y, we need a subdivision"
	self isMonoton ifFalse:[
		MonotonSubdivisions := MonotonSubdivisions + 1.
		^self subdivideToBeMonoton].

	"Test 2: If the receiver is horizontal, don't do anything"
	(end y = start y) ifTrue:[^nil].

	"Test 3: If the receiver can be represented as a straight line,
			make a line from the receiver and declare it invalid"
	((end - start) crossProduct: (via - start)) = 0 ifTrue:[
		LineConversions := LineConversions + 1.
		^self subdivideToBeLine].

	"Test 4: If the height of the curve exceeds 256 pixels, subdivide 
			(forward differencing is numerically not very stable)"
	dy := end y - start y.
	dy < 0 ifTrue:[dy := dy negated].
	(dy > 255) ifTrue:[
		HeightSubdivisions := HeightSubdivisions + 1.
		^self subdivideAt: 0.5].

	"Test 5: Check if the incremental values could possibly overflow the scaled integer range"
	dx := end x - start x.
	dx < 0 ifTrue:[dx := dx negated].
	dy * 32 < dx ifTrue:[
		OverflowSubdivisions := OverflowSubdivisions + 1.
		^self subdivideAt: 0.5].

	^nil