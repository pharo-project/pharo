Extension { #name : #RGElementDefinition }

{ #category : #'*Tool-Base' }
RGElementDefinition >> browse [

	^ Smalltalk tools browser openOnClass: self realClass
]
