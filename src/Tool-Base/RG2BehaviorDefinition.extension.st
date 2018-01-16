Extension { #name : #RG2BehaviorDefinition }

{ #category : #'*Tool-Base' }
RG2BehaviorDefinition >> browse [

	^ Smalltalk tools browser openOnClass: self realClass
]
