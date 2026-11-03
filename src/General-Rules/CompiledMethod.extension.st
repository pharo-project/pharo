Extension { #name : 'CompiledMethod' }

{ #category : '*General-Rules' }
CompiledMethod >> hasEquivalentMethodInSuperclass [
	"In case of a method from a trait, it is contextual to know if it overrides an equivalent method or not so we return false."

	self isFromTrait ifTrue: [ ^ false ].

	"Class initialize methods can be equivalent to their super method and it makes sense."
	(self selector = #initialize and: [ self isClassSide ]) ifTrue: [ ^ false ].

	^ self overriddenMethod
		  ifNil: [ false ]
		  ifNotNil: [ :method | self equivalentTo: method ]
]
