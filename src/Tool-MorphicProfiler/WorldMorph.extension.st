Extension { #name : 'WorldMorph' }

{ #category : '*Tool-MorphicProfiler' }
WorldMorph >> startThenBrowseMessageTally [
	"Tally only the UI process"

	(ConfirmationRequest signal: 'MessageTally the UI process until the
mouse pointer goes to the top of the screen') ifFalse: [ ^ self ].

	TimeProfiler onBlock: [ MorphicRenderLoop new doOneCycleWhile: [ self activeHand position y > 10 ] ]
]
