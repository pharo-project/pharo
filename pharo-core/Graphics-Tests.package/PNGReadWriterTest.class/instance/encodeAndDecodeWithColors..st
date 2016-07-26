helpers
encodeAndDecodeWithColors: aColorForm
	"Screw around with aColorForm colors"
	| colors nColors indexedColors max myRandom |
	fileName := 'testColors', aColorForm depth printString,'.png'.
	indexedColors := Color indexedColors.
	nColors := 1 bitShift: aColorForm depth.
	colors := Array new writeStream.

	"Make first half translucent"
	max := nColors // 2.
	1 to: max do:[:i|
		colors nextPut: ((indexedColors at: i) alpha: i / max asFloat).
	].

	"Make random choices for second half"
	myRandom := Random seed: 42315.
	max to: nColors do:[:i|
		colors nextPut: (indexedColors atRandom: myRandom).
	].
	self deleteFile.
