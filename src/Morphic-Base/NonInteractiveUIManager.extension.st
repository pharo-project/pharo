Extension { #name : #NonInteractiveUIManager }

{ #category : #'*Morphic-Base' }
NonInteractiveUIManager >> currentWorld [

	" err.. we should probably prohibit this message.. but lets play nice"
	
	uiManager ifNotNil: [ ^ uiManager currentWorld ].
	
	ActiveWorld ifNotNil: [^ActiveWorld].
	^World
]
