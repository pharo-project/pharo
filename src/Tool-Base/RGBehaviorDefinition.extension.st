Extension { #name : #RGBehaviorDefinition }

{ #category : #'*Tool-Base' }
RGBehaviorDefinition >> browse [

	^ Smalltalk tools browser openOnClass: self realClass
]
