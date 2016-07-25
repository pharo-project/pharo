instance creation
from: aKeyboardEvent 

	| instance |
	instance := self new.
	aKeyboardEvent shiftPressed
		ifTrue: [ instance selectNextTask ]
		ifFalse: [ instance selectPreviousTask ].
	^ instance.