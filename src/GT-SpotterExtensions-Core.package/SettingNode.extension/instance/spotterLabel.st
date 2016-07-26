*GT-SpotterExtensions-Core
spotterLabel
	| label |
	label := self settingDeclaration label.
	^ self parentNode isNil
		ifTrue: [ label ]
		ifFalse: [ self parentNode spotterLabel , '/', label ]
		