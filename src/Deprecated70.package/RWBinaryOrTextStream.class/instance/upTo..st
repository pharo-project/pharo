accessing
upTo: anObject 
	"Answer a subcollection from the current access position to the 
	occurrence (if any, but not inclusive) of anObject in the receiver. If 
	anObject is not in the collection, answer the entire rest of the receiver."
	| newStream element species |
	species := isBinary ifTrue:[ByteArray] ifFalse:[String].
	newStream := WriteStream on: (species new: 100).
	[self atEnd or: [(element := self next) = anObject]]
		whileFalse: [newStream nextPut: element].
	^newStream contents