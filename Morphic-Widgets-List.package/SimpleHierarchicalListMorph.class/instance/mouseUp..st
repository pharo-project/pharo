event handling
mouseUp: event 
	"Fixed up highlight problems."
	
	| aMorph wasHigh|
	aMorph := self itemFromPoint: event position.
	wasHigh := aMorph notNil ifTrue: [aMorph highlightedForMouseDown] ifFalse: [false].
	scroller submorphsDo: [:m |
		m highlightedForMouseDown ifTrue: [m highlightForMouseDown: false]].
	aMorph ifNil: [^self].
	wasHigh ifFalse: [^self].
	model okToChange ifFalse: [^self].
	"No change if model is locked"
	((autoDeselect == true) and: [aMorph == selectedMorph]) 
		ifTrue: [self setSelectedMorph: nil]
		ifFalse: [self setSelectedMorph: aMorph].
	Cursor normal show