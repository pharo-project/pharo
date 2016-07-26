initialization
loadFrom: anFT2Face
	"Same as in superclass, but avoid converting coordinates to not waste CPU cycles"
	
	face := anFT2Face.
	self primLoadFrom: anFT2Face.
	format := ((Smalltalk isLittleEndian) ifTrue: [ format reversed ] ifFalse: [ format ]) asString.
