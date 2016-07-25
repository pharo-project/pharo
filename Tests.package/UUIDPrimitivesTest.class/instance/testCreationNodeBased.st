tests
testCreationNodeBased
	

	(UUID new asString last: 12) = (UUID new asString last: 12) ifFalse: [^self].
	1000 timesRepeat:
		[ | uuid |uuid := UUID new.
		self should: [((uuid at: 7) bitAnd: 16rF0) = 16r10].
		self should: [((uuid at: 9) bitAnd: 16rC0) = 16r80]]
