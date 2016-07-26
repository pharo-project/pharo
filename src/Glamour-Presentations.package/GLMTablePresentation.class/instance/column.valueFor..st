private
column: aGlamourColumn valueFor: anObject
	^ aGlamourColumn computation glamourValue: 
		(anObject asGlamorousMultiValue,
		 self entity asGlamorousMultiValue,
		 self asGlamorousMultiValue)