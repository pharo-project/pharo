world state
viewBox: newViewBox
	"I am now displayed within newViewBox; react."

	(self viewBox isNil or: [ self viewBox extent ~= newViewBox extent ])
		ifTrue: [ worldState canvas: nil ].
	worldState viewBox: newViewBox.
	super viewBox: newViewBox.
	worldState handsDo: [ :hand | hand releaseKeyboardFocus ].
	self fullRepaintNeeded