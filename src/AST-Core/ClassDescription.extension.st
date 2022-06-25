Extension { #name : #ClassDescription }

{ #category : #'*AST-Core' }
ClassDescription >> formatter [

	^ self package packageManifestOrNil
		ifNotNil: [ :manifest | manifest formatter ]
		ifNil: [ RBProgramNode formatterClass new ]
]
