balloon help
deleteBalloonTarget: aMorph
	"Delete any existing balloon help.  This is now done unconditionally, whether or not the morph supplied is the same as the current balloon target"
	
	self balloonHelp: nil

"	| h |
	h := self balloonHelp ifNil: [^ self].
	h balloonOwner == aMorph ifTrue: [self balloonHelp: nil]"