Extension { #name : #ClyAsyncMethodGroup }

{ #category : #'*Calypso-SystemTools-Core' }
ClyAsyncMethodGroup >> decorateTableCell: anItemCellMorph of: groupItem [
	super decorateTableCell: anItemCellMorph of: groupItem.
	
	(groupItem isMarkedWith: ClyBackgroundProcessingTag) 
		ifTrue: [ anItemCellMorph definitionMorph: ClyActivityAnimationIconMorph new ]
		ifFalse: [methodQuery decorateMethodGroupTableCell: anItemCellMorph of: groupItem]
]
