Extension { #name : #GLMTablePresentation }

{ #category : #'*Versionner-Commit-UI' }
GLMTablePresentation >> column: aBlockOrString evaluated: aBlock modified: aModifiedBlock width: aNumber [
	self addColumn: (GLMTableColumn new 
		title: aBlockOrString;
		computation: aBlock;
		modifiedBlock: aModifiedBlock;
		width: aNumber)
]
