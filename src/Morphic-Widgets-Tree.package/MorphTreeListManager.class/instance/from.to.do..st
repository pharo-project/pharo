selection change
from: aNodeMorph to: anotherNodeMorph do: action
	| idx1 idx2 |
	idx1 := ((self getSelectionIndexOf: aNodeMorph) min: self allNodeMorphs size) max: 1.
	idx2 := ((self getSelectionIndexOf: anotherNodeMorph) min: self allNodeMorphs size) max: 1.
	(idx1 min: idx2) to: (idx1 max: idx2) do: [:idx | action value: (self allNodeMorphs at: idx)]
