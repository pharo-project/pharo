drop shadows
shadowOffset
	"Return the current shadow offset"
	
	extension ifNil: [^0@0].
	^self valueOfProperty: #shadowOffset ifAbsent:[0@0]