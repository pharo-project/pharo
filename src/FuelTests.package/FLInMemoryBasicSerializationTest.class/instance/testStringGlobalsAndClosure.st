tests
testStringGlobalsAndClosure

	| materialized |
	materialized := self resultOfSerializeAndMaterialize:  (Array 
			with: 'a string' 
			with: Transcript
			with: [ Transcript show: 'a string' ]).
	self assert: materialized first = 'a string'.
	self assert: materialized second == Transcript