Extension { #name : 'Association' }

{ #category : '*SUnit-Core' }
Association >> asTestParameter [

	^ ParametrizedTestExpandedParameter new
		selector: self key;
		valuable: self value;
		yourself
]
