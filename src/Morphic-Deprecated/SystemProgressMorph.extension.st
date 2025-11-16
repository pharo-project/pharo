Extension { #name : 'SystemProgressMorph' }

{ #category : '*Morphic-Deprecated' }
SystemProgressMorph class >> exampleLabelOnly [
	"SystemProgressMorph exampleLabelOnly"
	| words |
	words := #(zero one two three four five six seven eight nine ten) reversed.
	MorphicUIManager new informUserDuring: [:bar |
		words do: [:each|
			bar label: 'Countdown: ', each.
			(Delay forSeconds: 0.2) wait]]
]
