user interface
treeMorphIn: aMorph 
	| treeMorph |
	treeMorph := (self treeMorphClass on: self)
		getMenuSelector: #menu:shifted:;
		keystrokeActionSelector: #keyStroke:from:;
		columns: {
			MorphTreeColumn new startWidth: 300; rowMorphGetSelector: #labelMorphFor:; yourself.
			MorphTreeColumn new rowMorphGetSelector: #inputMorphFor:;yourself
		};
		doubleClickSelector: #doubleClick;
		rowInset: 6;
		columnInset: 6;
		rowColorForEven: Smalltalk ui theme lightBackgroundColor;
		yourself.
	^ treeMorph buildContents