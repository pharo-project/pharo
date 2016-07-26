initialization
buildRowMorph
	| rowControls colAndControls |
	controls := OrderedCollection new.
	colAndControls := container columns 
		collect: [:col | | v |
			v := col rowMorphFor: complexContents.
			controls add: v.
			col -> v].
	rowControls := OrderedCollection new.
	colAndControls
		do: [:ctrl | | col morph |
			col := ctrl key.
			morph := ctrl value.
			morph clipSubmorphs: true.
			morph vResizing: #shrinkWrap.  
			rowControls add: morph.
			(morph = controls last and: [container lastColumnUnbounded]) ifFalse: [morph hResizing: #rigid].
			(col resizable not and: [col shrinkWrap])
				ifTrue: [col currentWidth < morph width 
						ifTrue: [col forceWidthTo: morph width]]].

	self addAllMorphs: rowControls.
	self layoutChanged
	
