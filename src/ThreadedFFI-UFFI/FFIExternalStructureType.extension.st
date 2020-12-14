Extension { #name : #FFIExternalStructureType }

{ #category : #'*ThreadedFFI-UFFI' }
FFIExternalStructureType >> tfExternalType [
	^ TFStructType
		forClass: objectClass
		withMembers:
			(objectClass fieldSpec fields values
				flatCollect: [ :aField | aField tfExternalTypeForStructureWithArity ])
]

{ #category : #'*ThreadedFFI-UFFI' }
FFIExternalStructureType >> tfExternalTypeWithArity [

	self pointerArity = 1
		  ifTrue: [ 
			  ^ TFPointerToStructType new
				  targetClass: objectClass;
				  yourself ].
			
	^ self pointerArity > 1
		  ifTrue: [ TFBasicType pointer ]
		  ifFalse: [ self tfExternalType ]
]
