Extension { #name : #Association }

{ #category : #'*ParametrizedTests' }
Association >> asTestParameter [

	^ ParametrizedTestExpandedParameter new
		selector: self key;
		valuable: self value;
		yourself
]
