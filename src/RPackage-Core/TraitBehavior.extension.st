Extension { #name : #TraitBehavior }

{ #category : #'*Rpackage-Core' }
TraitBehavior >> originalName [
	^ ((self isObsolete and: [ self name beginsWith: 'AnObsolete' ])
		ifTrue: [ (self name copyFrom:   'AnObsolete' size + 1 to: self name size ) ]
		ifFalse:  [ self name ]) asSymbol
]
