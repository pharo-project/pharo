fields accessing
fieldOf: anObject at: anIndex put: newObject
	"Setting newObject into field at anIndex in order: indexable fields -> fixed fields"

	| fixedSize indexableSize |
	fixedSize := self fixedSizeOf: anObject.
	indexableSize := self indexableSizeOf: anObject.
	
	indexableSize = 0
		ifTrue: [ ^self fixedFieldOf: anObject at: anIndex put: newObject].
	fixedSize = 0
		ifTrue: [ ^self indexableFieldOf: anObject at: anIndex put: newObject].
			
	^anIndex > indexableSize
		ifTrue: [self fixedFieldOf: anObject at: anIndex - indexableSize put: newObject]
		ifFalse: [ self indexableFieldOf: anObject at: anIndex put: newObject]