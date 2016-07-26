enumerating
piecesCutWhere: binaryBlock
	"Answer substrings of the receiver derived from cutting the receiver at points where binaryBlock answers true for adjacent elements."

	| pieces |
	pieces := OrderedCollection new.
	self 
		piecesCutWhere: binaryBlock
		do: [ :piece | pieces add: piece ].
	^pieces

       "'Now is the time for all good people to come to the aid of the cause of world peace.  It is just fine, even desirable, to love your country, if that means wanting it to play a beneficial role in the course of world events and be the best possible example of a good society.  But if it means wanting dominion over the rest of the world, it is not love but defensiveness or self-glorification, and will lead only to oblivion.'  piecesCutWhere: [:a :b| a = $. and: [b isSeparator]]"