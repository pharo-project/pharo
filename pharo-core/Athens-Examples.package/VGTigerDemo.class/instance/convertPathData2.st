conversion
convertPathData2

	| paths pathData cmdStream ptStream fillBlocks strokeBlocks capBlocks joinBlocks cmdBlocks |
	
	paths := OrderedCollection new.
	cmdStream := self class commands readStream.
	ptStream := self class tigerPoints readStream.
	pathData := nil.
	
	fillBlocks := { 
		"N" [].
		"F" [ pathData addFill ].
		"E" [ pathData addFill; fillRule: #VGEVENODD ] }.

	strokeBlocks := {
		"N" [].
		"S" [ pathData addStroke ] }.
	
	capBlocks := {
		"B" [ pathData capStyle: #VGCAPBUTT ].
		"R" [ pathData capStyle: #VGCAPROUND ].
		"S" [ pathData capStyle: #VGCAPSQUARE ]	}.
	
	joinBlocks := {
		"M" [ pathData joinStyle: #VGJOINMITER ].
		"R" [ pathData joinStyle: #VGJOINROUND ].
		"B" [ pathData joinStyle: #VGJOINBEVEL ] }.
		
	cmdBlocks := {		
		"M"[:s | s moveToX: ptStream next Y: ptStream next ].
		"L" [:s | s lineToX: ptStream next Y: ptStream next ].
		"C"	[:s | s  
			curveVia: (ptStream next @ ptStream next)
			and: (ptStream next @ ptStream next)
			to: (ptStream next @ ptStream next) ].
		"E"	[:s | s close ] }.

	[		
		pathData := AthensTigerShape new.

		fillBlocks := { 
			"N" [].
			"F" [ pathData addFill; fillRule: #VGNONZERO.  ].
			"E" [ pathData addFill; fillRule: #VGEVENODD ] }.

		strokeBlocks := {
			"N" [].
			"S" [ pathData addStroke ] }.
		
		" fill type "
		pathData fillRule: #VGNONZERO.
		(fillBlocks at: ('NFE' indexOf: cmdStream next)) value.

		" stroke type "	
		(strokeBlocks at: ('NS' indexOf: cmdStream next)) value.

		" line cap "
		(capBlocks at: ('BRS' indexOf: cmdStream next)) value.
	
		" line join "
		(joinBlocks at: ('MRB' indexOf: cmdStream next)) value.
	
		" the rest of stroke attributes "
		pathData miterLimit: ptStream next.
		pathData strokeWidth: ptStream next.

		" paints "
		pathData strokePaint: ((Color r: ptStream next g: ptStream next b: ptStream next )).
		
		pathData fillPaint: ((Color r: ptStream next g: ptStream next b: ptStream next )).
		
		pathData setPath: (
			surface createPath: [:builder |
				builder absolute.
				(ptStream next asInteger) timesRepeat: [
					(cmdBlocks at: ('MLCE' indexOf: cmdStream next) ) value: builder.
				]
				]
		).

		paths add: pathData.
		cmdStream atEnd
	] whileFalse.

	paths do: [:ea |
		self assert: ea fillRule = #VGNONZERO.	
		ea prepareFor: surface
		].
	
	self assert: (ptStream atEnd).
	
	^ paths