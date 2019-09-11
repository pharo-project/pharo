Extension { #name : #EmptyLayout }

{ #category : #'*Ring-Core' }
EmptyLayout >> asRingMinimalDefinitionIn: anRGEnvironment [

	| aHost |
	
	aHost := self host 
		ifNotNil: [ :foundHost | foundHost asRingMinimalDefinitionIn: anRGEnvironment ]
		ifNil: [ self error: 'Cannot create model for an empty layout without a host'].

	^ anRGEnvironment backend definitionFor: self ifAbsentRegister: [
		RGEmptyLayout parent: aHost]

]
