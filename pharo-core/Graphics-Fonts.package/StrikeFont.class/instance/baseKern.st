accessing
baseKern
	"Return the base kern value to be used for all characters."
	
	| italic |

	italic := emphasis allMask: 2.	
	"Are we a bitmapped font with alpha, whose italic property is not synthetic?"
	(self isSyntheticItalic not and: [ glyphs depth > 1 ]) ifTrue: [
		^(italic or: [ pointSize < 9 ])
			ifTrue: [ 1 ]
			ifFalse: [ 0] ].
	
	italic ifFalse: [^ 0].
	^ ((self height-1-self ascent+4)//4 max: 0)  "See makeItalicGlyphs"
		+ (((self ascent-5+4)//4 max: 0))