analyzing
pixelCompare: aRect with: otherForm at: otherLoc
	"Compare the selected bits of this form (those within aRect) against
	those in a similar rectangle of otherFrom.  Return the sum of the
	absolute value of the differences of the color values of every pixel.
	Obviously, this is most useful for rgb (16- or 32-bit) pixels but,
	in the case of 8-bits or less, this will return the sum of the differing
	bits of the corresponding pixel values (somewhat less useful)"
	| pixPerWord temp |
	pixPerWord := 32//self depth.
	(aRect left\\pixPerWord = 0 and: [aRect right\\pixPerWord = 0]) ifTrue:
		["If word-aligned, use on-the-fly difference"
		^ (BitBlt toForm: self) copy: aRect from: otherLoc in: otherForm
				fillColor: nil rule: 32].
	"Otherwise, combine in a word-sized form and then compute difference"
	temp := self copy: aRect.
	temp copy: aRect from: otherLoc in: otherForm rule: 21.
	^ (BitBlt toForm: temp) copy: aRect from: otherLoc in: nil
				fillColor: (Bitmap with: 0) rule: 32
"  Dumb example prints zero only when you move over the original rectangle...
 | f diff | f := Form fromUser.
[Sensor anyButtonPressed] whileFalse:
	[diff := f pixelCompare: f boundingBox
		with: Display at: Sensor cursorPoint.
	diff printString , '        ' displayAt: 0@0]
"