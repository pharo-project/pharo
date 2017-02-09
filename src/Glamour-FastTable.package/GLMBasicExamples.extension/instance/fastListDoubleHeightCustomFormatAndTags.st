*Glamour-FastTable
fastListDoubleHeightCustomFormatAndTags
	"self new fastListDoubleHeightCustomFormatAndTags openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List With Double Height And Custom Format And Tags' input: 'Smalltalk allClassesAndTraits'>
	| browser |
	browser := GLMTabulator new.
	browser row: #Example.
	browser transmit
		to: #Example;
		andShow: [ :a | a fastList 
			display: Smalltalk allClassesAndTraits;
			rowHeight: [ :presentation | presentation class defaultRowHeight * 2 ];
			format: [ :aClass | 
				(Smalltalk ui theme newColumnIn: World for: { 
					aClass asStringMorph. 
					(' with ', aClass methods size printString, ' methods') asStringMorph })
						cellInset: 0; 
						hResizing: #shrinkWrap;
						vResizing: #shrinkWrap;
						yourself ];
			tags: [ :aClass | aClass methods size asString, ' methods' ] ].
	^ browser