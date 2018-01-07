Extension { #name : #Class }

{ #category : #'*Ring2-Compatibility' }
Class >> asRing2Definition [
	
	^ self ensureRing2DefinitionIn: RG2Environment new
]

{ #category : #'*Ring2-Compatibility' }
Class >> asRing2DefinitionIn: aRG2Environment [

	^ aRG2Environment ask behaviors detect: [:each | 
			each name = self name]  ifNone: [ RG2NotFoundError signal ]
]
