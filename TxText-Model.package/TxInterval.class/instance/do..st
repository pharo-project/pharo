enumeration
do: aBlock

	| iterator |
	end < start ifTrue:[^self].
	
	iterator := start copy.
		
	[end = iterator] whileFalse: [ 
		aBlock value: iterator. 
		iterator moveRight] 