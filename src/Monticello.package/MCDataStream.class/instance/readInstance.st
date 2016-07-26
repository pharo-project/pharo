write and read
readInstance
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "

	| instSize aSymbol refPosn anObject newClass |
	instSize := (byteStream nextNumber: 4) - 1.
	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := Smalltalk globals at: aSymbol asSymbol.
	anObject := newClass isVariable
		ifFalse: [ newClass basicNew ]
		ifTrue: [ newClass basicNew: instSize - newClass instSize ].	"Create object here"
	self setCurrentReference: refPosn.	"before readDataFrom:size:"
	anObject := anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.	"before returning to next"
	^ anObject