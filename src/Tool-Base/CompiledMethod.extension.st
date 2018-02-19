Extension { #name : #CompiledMethod }

{ #category : #'*Tool-Base' }
CompiledMethod >> browse [

	^ Smalltalk tools browser openOnClass: self methodClass selector: self selector
]
