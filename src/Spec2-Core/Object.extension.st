Extension { #name : #Object }

{ #category : #'*Spec2-Core' }
Object >> asPresenter [

	^ self asString asPresenter
]

{ #category : #'*Spec2-Core' }
Object >> isSpAnnouncingObject [

	^ false
]

{ #category : #'*Spec2-Core' }
Object >> isSpLayout [

	^ false
]
