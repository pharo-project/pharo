Extension { #name : #RGBehavior }

{ #category : #'*Refactoring-Environment' }
RGBehavior >> categoryOfElement: aSymbol ifAbsent: aBlockClosure [
	
	| aMethod |
	
	aMethod := (self localMethodNamed: aSymbol ifAbsent: nil).
	^ aMethod 
		ifNotNil: [ aMethod protocol ]
		ifNil: aBlockClosure
]
