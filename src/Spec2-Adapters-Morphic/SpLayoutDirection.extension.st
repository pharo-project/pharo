Extension { #name : #SpLayoutDirection }

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirection >> addPadding: aNumber toMorph: aMorph into: anAdapter [

	^ self subclassResponsibility
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirection >> newSplitter [
	^ self subclassResponsibility
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirection >> recalculatePages: anAdaptor [

	self subclassResponsibility
]

{ #category : #'*Spec2-Adapters-Morphic' }
SpLayoutDirection >> setRigidityOfNonExpandedMorph: aMorph [ 
	self subclassResponsibility
]
