Extension { #name : 'Array' }

{ #category : '*SUnit-Core' }
Array >> asTestMatrix [

	| matrix |
	matrix := ParametrizedTestMatrix new.
	self do: [ :e | matrix addCase: e ].
	^ matrix
]
