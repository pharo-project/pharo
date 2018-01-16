Extension { #name : #EmptyLayout }

{ #category : #'*Ring2-Core' }
EmptyLayout >> asRing2MinimalDefinitionIn: aRG2Environment [

	| aHost |
	
	aHost := self host 
		ifNotNil: [ :foundHost | foundHost asRing2MinimalDefinitionIn: aRG2Environment ]
		ifNil: [ self error: 'Cannot create model for an empty layout without a host'].

	^ aRG2Environment backend definitionFor: self ifAbsentRegister: [
		RG2EmptyLayoutDefinition parent: aHost]

]
