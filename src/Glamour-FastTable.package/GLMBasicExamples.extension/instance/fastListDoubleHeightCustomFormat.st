*Glamour-FastTable
fastListDoubleHeightCustomFormat
	"self new fastListDoubleHeightCustomFormat openOn: (Smalltalk allClassesAndTraits)"

	<glmBrowser: 'Fast List With Double Height And Custom Format' input: 'Smalltalk allClassesAndTraits'>
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
						cellInset: 0; yourself ] ].
	^ browser