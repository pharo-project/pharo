accessing
preferredColor

	| op |
	op := self operation.
	op ifNil: [ ^ nil ].
	(op isConflict and: [ op isResolved not ] ) ifTrue: [  ^ Color red ].
	op remoteChosen ifFalse: [ ^ Color gray ].
	^ nil "use default color "