Extension { #name : #IceOperationMerge }

{ #category : #'*Iceberg-TipUI' }
IceOperationMerge >> icon [

	self isRightChosen ifTrue: [ ^ self iconNamed: #changeBlock ].
	self isLeftChosen ifTrue: [ ^ self iconNamed: #forward ].
	
	^ self operationIcon
]

{ #category : #'*Iceberg-TipUI' }
IceOperationMerge >> operationIcon [
	self subclassResponsibility
]
