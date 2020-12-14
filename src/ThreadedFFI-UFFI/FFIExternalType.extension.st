Extension { #name : #FFIExternalType }

{ #category : #'*ThreadedFFI-UFFI' }
FFIExternalType >> tfExternalType [
		
	^ self subclassResponsibility
]

{ #category : #'*ThreadedFFI-UFFI' }
FFIExternalType >> tfExternalTypeForStructure [

	^ {self tfExternalType}
]

{ #category : #'*ThreadedFFI-UFFI' }
FFIExternalType >> tfExternalTypeForStructureWithArity [
	
	^ self pointerArity > 0 
		ifTrue: [ {TFBasicType pointer} ]
		ifFalse: [ self tfExternalTypeForStructure ]
]

{ #category : #'*ThreadedFFI-UFFI' }
FFIExternalType >> tfExternalTypeWithArity [
	
	^ self pointerArity > 0 
		ifTrue: [ TFBasicType pointer ]
		ifFalse: [ self tfExternalType ]
]
