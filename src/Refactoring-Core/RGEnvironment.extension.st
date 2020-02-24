Extension { #name : #RGEnvironment }

{ #category : #'*Refactoring-Core' }
RGEnvironment >> at: aString ifAbsent: aBlockClosure [ 
	| association |
	association := self bindingOf: aString.
	^ association 
		ifNotNil: [ association value ]
		ifNil: aBlockClosure
]
