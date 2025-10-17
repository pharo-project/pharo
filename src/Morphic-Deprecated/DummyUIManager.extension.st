Extension { #name : 'DummyUIManager' }

{ #category : '*Morphic-Deprecated' }
DummyUIManager >> requestPassword: queryString [

	^ self request: queryString initialAnswer: ''
]
