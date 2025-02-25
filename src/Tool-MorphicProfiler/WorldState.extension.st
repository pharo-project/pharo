Extension { #name : 'WorldState' }

{ #category : '*Tool-MorphicProfiler' }
WorldState class >> startMessageTally [

	(self confirm: 'MessageTally will start now,
and stop when the cursor goes
to the top of the screen' translated)
		ifFalse: [ ^ self ].

	TimeProfiler spyAllOn: [
		MorphicRenderLoop new doOneCycleWhile: [
			self currentWorld activeHand position y > 0 ] ]
]

{ #category : '*Tool-MorphicProfiler' }
WorldState >> startMessageTally [

	self class startMessageTally
]

{ #category : '*Tool-MorphicProfiler' }
WorldState class >> startThenBrowseMessageTally [
	"Tally only the UI process"

	(self confirm: 'MessageTally the UI process until the
mouse pointer goes to the top of the screen')
		ifFalse: [ ^ self ].

	TimeProfiler onBlock: [
		MorphicRenderLoop new doOneCycleWhile: [
			self currentWorld activeHand position y > 10 ] ]
]
