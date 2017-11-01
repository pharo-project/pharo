Extension { #name : #RGBehaviorDefinition }

{ #category : #'*NautilusCommon' }
RGBehaviorDefinition >> addCategory: newName before: aCategory [

	^ self addProtocol: newName
]
