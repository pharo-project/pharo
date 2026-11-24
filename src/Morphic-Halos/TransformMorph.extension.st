Extension { #name : 'TransformMorph' }

{ #category : '*Morphic-Halos' }
TransformMorph >> addCustomMenuItems: aCustomMenu hand: aHandMorph [
	super addCustomMenuItems: aCustomMenu hand: aHandMorph.
	smoothing = 1
		ifTrue: [aCustomMenu add: 'turn on smoothing' selector: #smoothingOn]
		ifFalse: [aCustomMenu add: 'turn off smoothing' selector: #smoothingOff]
]
