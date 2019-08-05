Extension { #name : #Array }

{ #category : #'*ParametrizedTests' }
Array >> asTestMatrix [

	| matrix |
	matrix := ParametrizedTestMatrix new.
	self do: [ :e | matrix addCase: e ].
	^ matrix
]
