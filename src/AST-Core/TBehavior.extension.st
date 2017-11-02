Extension { #name : #TBehavior }

{ #category : #'*ast-core' }
TBehavior >> parseTreeFor: aSymbol [	
	^ RBParser parseMethod: (self sourceCodeAt: aSymbol) onError: [ :msg :pos | ^ nil ]
]
