callbacks
accept: aText notifying: anObject 
	aText asString trimBoth ifEmpty: [^self].
	self text: aText.
	self changed: #text