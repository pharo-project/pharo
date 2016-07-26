search
basicKeyPressed: aChar
	| nextSelection milliSeconds slowKeyStroke oldSelection list |
	nextSelection := oldSelection := lastSelection.
	milliSeconds := Time millisecondClockValue.
	slowKeyStroke := milliSeconds - lastKeystrokeTime > 500.
	lastKeystrokeTime := milliSeconds.
	self searchedElement: nil.
	slowKeyStroke
		ifTrue: [ 
			"forget previous keystrokes and search in following elements"
			lastKeystrokes := aChar asLowercase asString ]
		ifFalse: [ 
			"append quick keystrokes but don't move selection if it still matches"
			lastKeystrokes := lastKeystrokes , aChar asLowercase asString ].	"Get rid of blanks and style used in some lists"	"No change if model is locked"
	model okToChange
		ifFalse: [ ^ self ].
	list := self allItems.
	list
		detect: [ :a | a searchingString trimBoth asLowercase beginsWith: lastKeystrokes ]
		ifFound: [ :nextSelectionNode | 
			nextSelection := list indexOf: nextSelectionNode ifAbsent: [ 0 ].	"The following line is a workaround around the behaviour of OBColumn>>selection:,
	 which deselects when called twice with the same argument."
			self searchedElement: nextSelectionNode.
			lastSelection := nextSelection.	"change scrollbarvalue"
			^ self vScrollValue: (nextSelection - 1) / list size ]