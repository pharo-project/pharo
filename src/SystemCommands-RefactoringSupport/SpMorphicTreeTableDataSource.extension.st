Extension { #name : #SpMorphicTreeTableDataSource }

{ #category : #'*SystemCommands-RefactoringSupport' }
SpMorphicTreeTableDataSource >> togglePath: aPath [ 
	(self itemAtPath: aPath) ifNotNil:[ :aNode | 
		aNode toggle.
		self tableRefresh ]
]
