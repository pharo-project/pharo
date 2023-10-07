Extension { #name : 'ClassDescription' }

{ #category : '*AST-Core' }
ClassDescription >> formatter [

	^ self package packageManifestOrNil formatter
]
