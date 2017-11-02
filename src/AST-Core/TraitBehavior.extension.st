Extension { #name : #TraitBehavior }

{ #category : #'*ast-core' }
TraitBehavior >> parseTreeFor: aSymbol [	
	^ RBParser parseMethod: (self sourceCodeAt: aSymbol) onError: [ :msg :pos | ^ nil ]
]
