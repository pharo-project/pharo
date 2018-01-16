Extension { #name : #RG2MethodDefinition }

{ #category : #'*Tool-Base' }
RG2MethodDefinition >> browse [

	^ Smalltalk tools browser
		openOnClass: self realClass 
		selector: (self isDefined ifTrue: [ self selector ] ifFalse: [ nil ])
]
