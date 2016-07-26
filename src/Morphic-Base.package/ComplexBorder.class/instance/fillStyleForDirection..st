private
fillStyleForDirection: direction
	"Fill the given form describing the receiver's look at a particular direction"
	| index fill dir |
	index := direction degrees truncated // 10 + 1.
	lineStyles ifNotNil:[
		fill := lineStyles at: index.
		fill ifNotNil:[^fill].
	].
	dir := Point r: 1.0 degrees: index - 1 * 10 + 5.
	fill := GradientFillStyle colors: (self colorsForDirection: dir).
	fill direction: 0 @ width asPoint y; radial: false.
	fill origin: ((width asPoint x // 2) @ (width asPoint y // 2)) negated.
	fill pixelRamp: (fill computePixelRampOfSize: 16).
	fill isTranslucent. "precompute"
	lineStyles ifNil:[lineStyles := Array new: 37].
	lineStyles at: index put: fill.
	^fill