accessing
project
	| myparent |
	myparent := self parent.
	[ myparent isNotNil ] whileTrue: [ [ myparent := myparent parent ] on: MessageNotUnderstood do: [ ^ myparent ] ].
	^ myparent