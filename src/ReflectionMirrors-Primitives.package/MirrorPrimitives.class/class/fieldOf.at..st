fields accessing
fieldOf: anObject at: anIndex
	"Answer field at anIndex in order: indexable fields -> fixed fields"

	| fixedSize indexableSize |
	fixedSize := self fixedSizeOf: anObject.
	indexableSize := self indexableSizeOf: anObject.
	
	indexableSize = 0
		ifTrue: [ ^self fixedFieldOf: anObject at: anIndex].
	fixedSize = 0
		ifTrue: [ ^self indexableFieldOf: anObject at: anIndex].
		
	^anIndex > indexableSize
		ifTrue: [self fixedFieldOf: anObject at: anIndex - indexableSize ]
		ifFalse: [ self indexableFieldOf: anObject at: anIndex ]