Extension { #name : 'DummyUIManager' }

{ #category : '*Morphic-Deprecated' }
DummyUIManager >> informUserDuring: aBlock [

	aBlock value: DummySystemProgressItem new
]

{ #category : '*Morphic-Deprecated' }
DummyUIManager >> requestPassword: queryString [

	^ self request: queryString initialAnswer: ''
]
