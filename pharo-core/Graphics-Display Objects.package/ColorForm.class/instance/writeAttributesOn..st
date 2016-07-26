filein/out
writeAttributesOn: file
	| colorArray |
	super writeAttributesOn: file.
	colorArray := self colors asColorArray.
	1 to: (2 raisedTo: depth) do: [:idx |
		file nextLittleEndianNumber: 4 put: (colorArray basicAt: idx).
	] 