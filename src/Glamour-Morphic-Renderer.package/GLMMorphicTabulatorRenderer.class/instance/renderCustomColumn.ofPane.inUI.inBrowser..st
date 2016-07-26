private
renderCustomColumn: aCell ofPane: aPane inUI: aMorph inBrowser: aBrowser
	| pane totalSpans currentSpanPosition currentOffset |
	totalSpans := aCell children inject: 0 into: [ :sum :each | sum + each span ].
	currentSpanPosition := 0.
	currentOffset := 0.
	aCell children
		keysAndValuesDo: [ :index :each | 
			each hasId
				ifTrue: [ pane := self renderObject: (aBrowser paneNamed: each id) ]
				ifFalse: [ 
					pane := GLMMorphic containerMorph.
					self
						renderCustomRow: each
						ofPane: aPane
						inUI: pane
						inBrowser: aBrowser.
					pane addPaneSplitters ].
			pane
				layoutFrame:
					(LayoutFrame new
						topFraction: currentSpanPosition / totalSpans offset: currentOffset + self margin;
						leftFraction: 0 offset: 0;
						bottomFraction: (currentSpanPosition + each span) / totalSpans offset: currentOffset + each size - self margin;
						rightFraction: 1 offset: 0;
						yourself).
			aMorph addMorphBack: pane.
			currentSpanPosition := currentSpanPosition + each span.
			currentOffset := currentOffset + each size ].
	aCell children last size > 0
		ifTrue: [ 
			(aMorph submorphs atLast: 2) layoutFrame
				bottomOffset: (aMorph submorphs atLast: 2) layoutFrame bottomOffset - currentOffset.
			aMorph submorphs last layoutFrame topOffset: aMorph submorphs last layoutFrame topOffset - currentOffset.
			aMorph submorphs last layoutFrame
				bottomOffset: aMorph submorphs last layoutFrame bottomOffset - currentOffset ].
	aMorph addPaneSplitters