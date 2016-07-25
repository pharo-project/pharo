private
renderCustomRow: aCell ofPane: aPane inUI: aMorph inBrowser: aBrowser 
	| pane totalSpans currentSpanPosition currentOffset |
	totalSpans := aCell children inject: 0 into: [ :sum :each | sum + each span ].
	currentSpanPosition := 0.
	currentOffset := 0.
	aCell children keysAndValuesDo: [ :index :each | 
		each hasId 
			ifTrue: [ pane := self renderObject: (aBrowser paneNamed: each id) ]
			ifFalse: [ 
				pane := GLMMorphic containerMorph.
				self 
					renderCustomColumn: each
					ofPane: aPane
					inUI: pane
					inBrowser: aBrowser.
				pane addPaneSplitters ].
		pane layoutFrame: (LayoutFrame new
				topFraction: 0 offset: 0;
				leftFraction: currentSpanPosition / totalSpans offset: currentOffset + self margin;
				bottomFraction: 1 offset: 0;
				rightFraction: (currentSpanPosition + each span) / totalSpans offset: currentOffset + each size - self margin;
				yourself).
		aMorph addMorphBack: pane.
		currentSpanPosition := currentSpanPosition + each span.
		currentOffset := currentOffset + each size ].
	(aCell children last size > 0) ifTrue: [
		(aMorph submorphs atLast: 2) layoutFrame rightOffset: (aMorph submorphs atLast: 2) layoutFrame rightOffset - currentOffset. 
		aMorph submorphs last layoutFrame leftOffset: aMorph submorphs last layoutFrame leftOffset - currentOffset. 
		aMorph submorphs last layoutFrame rightOffset: aMorph submorphs last layoutFrame rightOffset - currentOffset.].
	aMorph addPaneSplitters