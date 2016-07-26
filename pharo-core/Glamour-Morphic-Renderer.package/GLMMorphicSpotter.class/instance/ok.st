accessing
ok
	| nonEmpty |
	nonEmpty := self childrenMorphs reject: [ :e | e listIndex isZero ].
	nonEmpty size = 1
		ifTrue: [ answer := nonEmpty first listSelection ].
	^ super ok