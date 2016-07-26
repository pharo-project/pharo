color mapping
reducedPaletteOfSize: nColors
	"Return an array of colors of size nColors, such that those colors
	represent well the pixel values actually found in this form."
	| threshold tallies colorTallies dist delta palette cts top cluster |
	tallies := self tallyPixelValues.  "An array of tallies for each pixel value"
	threshold := width * height // 500.

	"Make an array of (color -> tally) for all tallies over threshold"
	colorTallies := Array streamContents:
		[:s | tallies withIndexDo:
			[:v :i | v >= threshold ifTrue:
				[s nextPut: (Color colorFromPixelValue: i-1 depth: depth) -> v]]].

	"Extract a set of clusters by picking the top tally, and then removing all others
	whose color is within dist of it.  Iterate the process, adjusting dist until we get nColors."
	dist := 0.2.  delta := dist / 2.
		[cts := colorTallies copy.
		palette := Array streamContents: [:s |
			[cts isEmpty] whileFalse:
				[top := cts detectMax: [:a | a value].
				cluster := cts select: [:a | (a key diff: top key) < dist].
				s nextPut: top key -> (cluster detectSum: [:a | a value]).
				cts := cts copyWithoutAll: cluster]].
		palette size = nColors or: [delta < 0.001]]
		whileFalse:
			[palette size > nColors
				ifTrue: [dist := dist + delta]
				ifFalse: [dist := dist - delta].
			delta := delta / 2].
	^ palette collect: [:a | a key]
