writing
writePresentInitializers
	| orderedClasses presentInitializers |
	presentInitializers := self presentInitializers.
	orderedClasses := (Class
		superclassOrder: (presentInitializers collect: [ :each | Smalltalk classOrTraitNamed: each key ]))
		collect: [ :each | each name ].
	orderedClasses
		do: [ :className | stream nextPutAll: (presentInitializers detect: [ :each | each key = className ]) value contents ]