Extension { #name : 'WorldState' }

{ #category : '*Tool-MorphicProfiler' }
WorldState class >> startMessageTally [

	(self confirm: 'MessageTally will start now,
and stop when the cursor goes
to the top of the screen')
		ifFalse: [ ^ self ].

	TimeProfiler spyAllOn: [
		MorphicRenderLoop new doOneCycleWhile: [
			self currentWorld activeHand position y > 0 ] ]
]

{ #category : '*Tool-MorphicProfiler' }
WorldState >> startMessageTally [

	self class startMessageTally
]
