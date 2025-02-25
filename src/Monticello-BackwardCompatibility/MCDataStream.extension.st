Extension { #name : 'MCDataStream' }

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readBitmap [
	"PRIVATE -- Read the contents of a Bitmap."
	^ Bitmap newFromStream: byteStream
	"Note that the reader knows that the size is in long words, but the data is in bytes."
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readByteArray [
	"PRIVATE -- Read the contents of a ByteArray."

	| count |
	count := byteStream nextNumber: 4.
	^ byteStream next: count  "assume stream is in binary mode"

]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readClass [
	^ self error: 'Classes cannot be materialized with DataStream'
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readFloat [
	"PRIVATE -- Read the contents of a Float.
	 This is the fast way to read a Float.
	 We support 8-byte Floats here.  Non-IEEE"

	| new |
	new := Float new: 2.		"To get an instance"
	new at: 1 put: (byteStream nextNumber: 4).
	new at: 2 put: (byteStream nextNumber: 4).
	^ new
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readMethod [
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "

	| instSize newClass className xxHeader nLits byteCodeSizePlusTrailer newMethod lits |
	instSize := (byteStream nextNumber: 4) - 1.
	className := self next.
	newClass := Smalltalk globals at: className asSymbol.
	xxHeader := self next.	"nArgs := (xxHeader >> 24) bitAnd: 16rF."	"nTemps := (xxHeader >> 18) bitAnd: 16r3F."	"largeBit := (xxHeader >> 17) bitAnd: 1."
	nLits := xxHeader >> 9 bitAnd: 16rFF.	"primBits := ((xxHeader >> 19) bitAnd: 16r600) + (xxHeader bitAnd: 16r1FF)."
	byteCodeSizePlusTrailer := instSize - newClass instSize - ((nLits + 1) * 4).	"0"
	newMethod := newClass basicNew: byteCodeSizePlusTrailer header: xxHeader.
	lits := newMethod numLiterals + 1.	"counting header"
	2 to: lits do: [ :ii | newMethod objectAt: ii put: self next ].
	lits * 4 + 1 to: newMethod basicSize do: [ :ii | newMethod basicAt: ii put: byteStream next ].	"Get raw bytes directly from the file"
	^ newMethod
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readRectangle [
    "Read a compact Rectangle.  Rectangles with values outside +/- 2047 were stored as normal objects (type=9).  They will not come here.  17:22 tk"

	"Encoding is four 12-bit signed numbers.  48 bits in next 6 bytes.  17:24 tk"
	| acc left top right bottom |
	acc := byteStream nextNumber: 3.
	left := acc bitShift: -12.
	(left bitAnd: 16r800) ~= 0 ifTrue: [left := left - 16r1000].	"sign"
	top := acc bitAnd: 16rFFF.
	(top bitAnd: 16r800) ~= 0 ifTrue: [top := top - 16r1000].	"sign"

	acc := byteStream nextNumber: 3.
	right := acc bitShift: -12.
	(right bitAnd: 16r800) ~= 0 ifTrue: [right := right - 16r1000].	"sign"
	bottom := acc bitAnd: 16rFFF.
	(bottom bitAnd: 16r800) ~= 0 ifTrue: [bottom := bottom - 16r1000].	"sign"
	
    ^ Rectangle left: left right: right top: top bottom: bottom

]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readReference [
	"Read the contents of an object reference. (Cf. outputReference:)  File is not now positioned at this object."
	| referencePosition |
	^ (referencePosition := (byteStream nextNumber: 4)) = self vacantRef	"relative"
		ifTrue:  [nil]
		ifFalse: [self objectAt: referencePosition]		"relative pos"
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readShortInst [
	"Read the contents of an arbitrary instance that has a short header.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "

	| instSize aSymbol anObject newClass |
	instSize := byteStream next - 1.	"one byte of size"
	aSymbol := self readShortRef.	"class symbol in two bytes of file pos"
	newClass := Smalltalk globals at: aSymbol asSymbol.
	anObject := newClass isVariable
		ifFalse: [ newClass basicNew ]
		ifTrue: [ newClass basicNew: instSize - newClass instSize ].	"Create object here"
	anObject := anObject readDataFrom: self size: instSize.
	^ anObject
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readShortRef [
	"Read an object reference from two bytes only.  Original object must be in first 65536 bytes of the file.  Relative to start of data.  vacantRef not a possibility."

	^ self objectAt: (byteStream nextNumber: 2)
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readStringOld [

   ^ byteStream nextStringOld
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readUser [
	"Reconstruct both the private class and the instance.  Still used??"
	^ self readInstance.		"Will create new unique class"

]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> readWordArray [
	"PRIVATE -- Read the contents of a WordArray."
	^ WordArray newFromStream: byteStream
	"Size is number of long words."
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeBitmap: aBitmap [
	"PRIVATE -- Write the contents of a Bitmap."

	aBitmap writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words."
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeByteArray: aByteArray [
	"PRIVATE -- Write the contents of a ByteArray."

	byteStream nextNumber: 4 put: aByteArray size.
	"May have to convert types here..."
	byteStream nextPutAll: aByteArray.
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeClass: aClass [
    ^ self error: 'Classes cannot be serialized with DataStream'
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeFloat: aFloat [
	"PRIVATE -- Write the contents of a Float.
	  We support 8-byte Floats here."

	byteStream nextNumber: 4 put: (aFloat at: 1).
	byteStream nextNumber: 4 put: (aFloat at: 2).

]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeRectangle: anObject [
    "Write the contents of a Rectangle.  See if it can be a compact Rectangle (type=15).  Rectangles with values outside +/- 2047 were stored as normal objects (type=9).  17:22 tk"

	| ok right bottom top left acc |
	ok := true.
	(right := anObject right) > 2047 ifTrue: [ok := false].
	right < -2048 ifTrue: [ok := false].
	(bottom := anObject bottom) > 2047 ifTrue: [ok := false].
	bottom < -2048 ifTrue: [ok := false].
	(top := anObject top) > 2047 ifTrue: [ok := false].
	top < -2048 ifTrue: [ok := false].
	(left := anObject left) > 2047 ifTrue: [ok := false].
	left < -2048 ifTrue: [ok := false].
	ok := ok & left isInteger & right isInteger & top isInteger & bottom isInteger.

	ok ifFalse: [
		byteStream skip: -1; nextPut: 9; skip: 0. "rewrite type to be normal instance"
	    ^ anObject storeDataOn: self].

	acc := ((left bitAnd: 16rFFF) bitShift: 12) + (top bitAnd: 16rFFF).
	byteStream nextNumber: 3 put: acc.
	acc := ((right bitAnd: 16rFFF) bitShift: 12) + (bottom bitAnd: 16rFFF).
	byteStream nextNumber: 3 put: acc.
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeStringOld: aString [
	"PRIVATE -- Write the contents of a String."

	| length |
	aString size < 16384 
		ifTrue: [
			(length := aString size) < 192
				ifTrue: [byteStream nextPut: length]
				ifFalse: 
					[byteStream nextPut: (length // 256 + 192).
					byteStream nextPut: (length \\ 256)].
			aString do: [:char | byteStream nextPut: char asciiValue]]
		ifFalse: [self writeByteArray: aString].	"takes more space"
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeUser: anObject [
    "Write the contents of an arbitrary User instance (and its devoted class)."

	"If anObject is an instance of a unique user class, will lie and say it has a generic class"
    ^ anObject storeDataOn: self
]

{ #category : '*Monticello-BackwardCompatibility' }
MCDataStream >> writeWordLike: aWordArray [
	"Note that we put the class name before the size."

	self nextPut: aWordArray class name.
	aWordArray writeOn: byteStream
	"Note that this calls (byteStream nextPutAll: aBitmap) which knows enough to put 4-byte quantities on the stream!  Reader must know that size is in long words or double-bytes."
]
