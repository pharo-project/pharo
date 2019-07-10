Extension { #name : #SpAbstractMorphicAdapter }

{ #category : #'*Spec2-Deprecated' }
SpAbstractMorphicAdapter >> setModal: aWindow [ 

	self widgetDo: [ :w | w spSetModal: aWindow ]
]
