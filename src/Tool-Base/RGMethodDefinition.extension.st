Extension { #name : #RGMethodDefinition }

{ #category : #'*Tool-Base' }
RGMethodDefinition >> browse [

	^ Smalltalk tools browser
		openOnClass: self realClass 
		selector: (self isDefined ifTrue: [ self selector ] ifFalse: [ nil ])
]
