Extension { #name : #Behavior }

{ #category : #'*ast-core' }
Behavior >> parseTreeFor: aSymbol [	
	^ RBParser parseMethod: (self sourceCodeAt: aSymbol) onError: [ :msg :pos | ^ nil ]
]
