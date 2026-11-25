Extension { #name : 'AlignmentMorph' }

{ #category : '*Morphic-Widgets' }
AlignmentMorph >> openInWindowLabeled: aString inWorld: aWorld [

	self layoutInset: 0.
	^ super openInWindowLabeled: aString inWorld: aWorld
]
