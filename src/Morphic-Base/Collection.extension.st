Extension { #name : #Collection }

{ #category : #'*Morphic-Base-Basic' }
Collection >> asDraggableMorph [
	^ (String streamContents: [ :s| 
		self 
			do: [ :each | s print: each ]
			separatedBy: [ s space ]]) asStringMorph
]
