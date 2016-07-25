accessing
currentCharacterStyleOrNil
	"Answer the CharacterStyle for the current selection or cursor location if any"

	^self hasSelection
		ifTrue: [ self text characterStyleOrNilAt: self startIndex ]
		ifFalse: [ self text characterStyleOrNilIfApplying: self emphasisHere ]