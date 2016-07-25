nil
minExtentHorizontal: aMorph
	"Return the minimal size aMorph's children would require given the new bounds"
	| inset minX minY maxX maxY n size width height |
	size := properties minCellSize asPoint. minX := size x. minY := size y.
	size := properties maxCellSize asPoint. maxX := size x. maxY := size y.
	inset := properties cellInset asPoint.
	n := 0.
	width := height := 0.
	aMorph submorphsDo:[:m| | sizeY sizeX |
		m disableTableLayout ifFalse:[
			n := n + 1.
			size := m minExtent. sizeX := size x. sizeY := size y.
			sizeX < minX
				ifTrue:[sizeX := minX]
				ifFalse:[sizeX > maxX ifTrue:[sizeX := maxX]].
			sizeY < minY
				ifTrue:[sizeY := minY]
				ifFalse:[sizeY > maxY ifTrue:[sizeY := maxY]].
			width := width + sizeX.
			sizeY > height ifTrue:[height := sizeY].
		].
	].
	n > 1 ifTrue:[width := width + (n-1 * inset x)].
	^minExtentCache := width @ height