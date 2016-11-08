user interface
treeMorph
	| treeMorph baseColor oddColor evenColor |
	baseColor := Color lightBlue lighter.
	oddColor := (GradientFillStyle
		ramp:
			{(0.0 -> baseColor whiter).
			(0.2 -> baseColor).
			(0.8 -> baseColor darker).
			(1.0 -> baseColor blacker)}) radial: false.
	baseColor := Color veryLightGray muchLighter.
	evenColor := (GradientFillStyle
		ramp:
			{(0.0 -> baseColor lighter lighter).
			(0.2 -> baseColor lighter).
			(0.8 -> baseColor).
			(1.0 -> baseColor blacker)}) radial: false.
	treeMorph := self treeMorphClass new
		model: self;
		beMultiple;
		columns:
			{(MorphTreeColumn new
				startWidth: 100;
				rowMorphGetSelector: #classButton;
				headerButtonLabel: 'Class'
					font: nil
					icon: (self iconNamed: #smallOpenIcon)
					target: nil
					actionSelector: nil
					arguments: #()).
			(MorphTreeColumn new
				rowMorphGetSelector: #commentText;
				headerButtonLabel: 'Comments'
					font: nil
					icon: (self iconNamed: #smallPrintIcon)
					target: nil
					actionSelector: nil
					arguments: #())};
		makeLastColumnUnbounded;
		withHLines: true;
		allowColumnDrop;
		useSquareCorners;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		rowInset: 5;
		columnInset: 5;
		resizerWidth: 2;
		preferedPaneColor: Color white;
		getMenuSelector: #menu:shifted:;
		columnColorForEven: evenColor odd: oddColor.
	^ treeMorph buildContents