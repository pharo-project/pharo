construction
title: aString

	titleContainer ifNotNil: [ titleContainer delete ].
	
	titleContainer := AlignmentMorph newColumn.
	titleContainer 
	color: Color transparent;
	hResizing: #spaceFill;
	vResizing: #shrinkWrap;
	wrapCentering: #center;
	layoutInset: 0;
	layoutChanged.
	
	aString translated linesDo: [ :line |
		titleContainer addMorphBack: (StringMorph contents: line font: StandardFonts menuFont)
	].
	
	pinBox ifNotNil: [ ^self addMorph: titleContainer inFrontOf: pinBox ].
	
	self addMorphBack: titleContainer 