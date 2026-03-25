Extension { #name : 'CompiledMethod' }

{ #category : '*Tools' }
CompiledMethod >> browse [

	^ Smalltalk tools browser openOnClass: self methodClass selector: self selector
]
