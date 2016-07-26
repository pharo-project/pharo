private
emphasisForCode: aCode
	^ {(#bold -> 1).
	(#italic -> 2).
	(#underline -> 4).
	(#narrow -> 8).
	(#struck -> 16)}
		collect: [ :pair | 
			(aCode bitAnd: pair value) = 0
				ifFalse: [ pair key ] ]
		thenReject: #isNil