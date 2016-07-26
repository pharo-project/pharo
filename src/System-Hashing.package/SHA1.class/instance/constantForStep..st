private
constantForStep: i
	"Answer the constant for the i-th step of the block hash loop. We number our steps 1-80, versus the 0-79 of the standard."

	i <= 20 ifTrue: [^ K1].
	i <= 40 ifTrue: [^ K2].
	i <= 60 ifTrue: [^ K3].
	^ K4
