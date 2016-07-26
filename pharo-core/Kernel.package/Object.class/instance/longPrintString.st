printing
longPrintString
	"Answer a String whose characters are a description of the receiver."
	
	| str |
	str := String streamContents: [:aStream | self longPrintOn: aStream].
	"Objects without inst vars should return something"
	^ str isEmpty ifTrue: [self printString, String cr] ifFalse: [str]