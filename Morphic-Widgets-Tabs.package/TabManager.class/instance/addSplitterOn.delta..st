private-contents
addSplitterOn: tabs delta: delta
	| leftProcess rightProcess |
	tabs overlappingPairsWithIndexDo: [ :left :right :index | 
		leftProcess := left retrieveMorph: [ :leftContents | 
			leftContents ifNil: [ TabWithNoContents signalTab: left ].
			rightProcess := right retrieveMorph: [ :rightContents || splitter |
				rightContents ifNil: [ TabWithNoContents signalTab: right ].
				
				splitter := ProportionalSplitterMorph new
					addLeftOrTop: leftContents;
					addRightOrBottom: rightContents;
					yourself.

				contentsWrapper
					addMorph: splitter
					fullFrame:
						(LayoutFrame identity
							leftFraction: index * delta;
							rightFraction: index * delta;
							leftOffset: -2;
							rightOffset: 2) ] ] ].
				
	leftProcess ifNotNil: [ processes add: leftProcess ].
	rightProcess ifNotNil: [ processes add: rightProcess ].