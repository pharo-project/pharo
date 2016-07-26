user interface
morphicGrid
	grid := FileListGrid new
		parent: self.
	grid
		onSelectionChangeSend: #findFileListIndex: to: self ;
		menu: #fileListMenu  shifted: false.
	^ grid treeMorph
