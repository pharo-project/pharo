examples - branches
exampleCascadeIfFalse
	| tmp1 |
	tmp1 := true value
		not;
		and: [ false ];
		yourself.
	tmp1
		ifFalse: [ ^ false ]