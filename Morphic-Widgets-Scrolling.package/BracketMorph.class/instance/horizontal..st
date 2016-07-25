accessing
horizontal: aBoolean
	"Set whether horizontal or vertical."
	
	^self orientation: (aBoolean ifTrue: [#horizontal] ifFalse: [#vertical])