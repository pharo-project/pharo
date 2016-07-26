private
noCheckAdd: anAssociation
	"Add anAssociation to the receiver. Discard expired associations. Put nil keys at the beginning."
	
	| key |
	
	key := anAssociation key. "Hold on to the key during this method"

	anAssociation expired ifFalse:[
		key ifNotNil:[
			super noCheckAdd: anAssociation
		] ifNil: [
			1 to: array size do:[:n|
				(array at: n) ifNil:[
					array at: n put: anAssociation. 
					tally := tally + 1.
					^self
				].
			].
			self error: 'could not find an empty slot.'
		].
	].