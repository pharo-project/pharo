mathematical functions
raisedToInteger: anInteger

	"The 0 raisedToInteger: 0 is an special case. In some contexts must be 1 and in others must
	be handled as an indeterminate form.
	I take the first context because that's the way that was previously handled.
	Maybe further discussion is required on this topic."
	
	|bitProbe result|

	anInteger negative ifTrue: [^(self raisedToInteger: anInteger negated) reciprocal].
	bitProbe := 1 bitShift: anInteger highBit - 1.
 	result := self class one.
  	[
		(anInteger bitAnd: bitProbe) = 0 ifFalse: [result := result * self].
       bitProbe := bitProbe bitShift: -1.
		bitProbe > 0 ]
	whileTrue: [result := result * result].
	
	^result