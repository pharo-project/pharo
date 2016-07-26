accessing
text: aString
	text = aString
		ifTrue: [ ^ self ].
	text := aString.
	self childrenMorphs do: [:each | each text: aString ].
	self changed: #text; changed: #isEnabled