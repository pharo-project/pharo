logical operations
==> aBlock
	"The material conditional, also known as the material implication or truth functional conditional.
	Correspond to not ... or ... and does not correspond to the English if...then... construction.
		
	 known as:
			b if a 
			a implies b
			if a then b
			b is a consequence of a
			a therefore b (but note: 'it is raining therefore it is cloudy' is implication; 'it is autumn therefore the leaves are falling' is equivalence).
		
	Here is the truth table for material implication:
	
	   p   |   q   |   p ==> q
	-------|-------|-------------
	   T   |   T   |      T
	   T   |   F   |      F
	   F   |   T   |      T
	   F   |   F   |      T
	"

	^self not or: [aBlock value]