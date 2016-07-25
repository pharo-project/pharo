new selection
correctFrom: start to: stop with: aString
	"Make a correction in the model that the user has authorised from somewhere else in the system (such as from the compilier).  
	The user's selection is not changed, only corrected."
	| wasShowing userSelection delta loc |
	
	userSelection := self selectionInterval.
	self selectInvisiblyFrom: start to: stop.
	self replaceSelectionWith: aString asText.
	delta := aString size - (stop - start + 1).
	self selectInvisiblyFrom:
		userSelection first + (userSelection first > start ifFalse: [ 0 ] ifTrue: [ delta ])
		to: userSelection last + (userSelection last > start ifFalse: [ 0 ] ifTrue: [ delta ]).
