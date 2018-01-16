Extension { #name : #RG2BehaviorDefinition }

{ #category : #'*NautilusCommon' }
RG2BehaviorDefinition >> addCategory: newName before: aCategory [

	^ self addProtocol: newName
]
