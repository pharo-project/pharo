*Morphic-Base-Basic
asDraggableMorph
	^ (String streamContents: [ :s| 
		self 
			do: [ :each | s print: each ]
			separatedBy: [ s space ]]) asStringMorph