Extension { #name : #Object }

{ #category : #'*Spec-Core' }
Object >> asWidget [

	^ self
]

{ #category : #'*Spec-Core' }
Object >> isAnnouncingObject [

	^ false
]

{ #category : #'*Spec-Core' }
Object >> isSpecAdapter [

	^ false
]

{ #category : #'*Spec-Core' }
Object >> isSpecContainer [
	^ false
]

{ #category : #'*Spec-Core' }
Object >> isSpecLayout [

	^ false
]

{ #category : #'*Spec-Core' }
Object >> isTreeNodeModel [

	^ false
]

{ #category : #'*Spec-Core' }
Object >> wrappedInValueHolderIfNeeded [

	^ self asValueHolder
]
