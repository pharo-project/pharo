user interface
treeMorph
	| treeMorph oddColor evenColor |
	treeMorph := (self treeMorphClass on: self)
		beCheckList;
		beMultiple;
		columns: {MorphTreeColumn new startWidth: 100;
						rowMorphGetSelector: #firstNameMorph;
						headerButtonLabel: 'First name' font:  nil.
					MorphTreeColumn new startWidth: 150;
						rowMorphGetSelector: #secondNameMorph;
						headerButtonLabel: 'Second name' font:  nil.
					MorphTreeColumn new startWidth: 50;
						rowMorphGetSelector: #ageMorph;
						headerButtonLabel: 'Age' font:  nil.
					MorphTreeColumn new startWidth: 50;
						rowMorphGetSelector: #marriedMorph;
					 	headerButtonLabel: 'Married' font: nil};
		columnInset: 3;
		rowInset: 1;
		preferedPaneColor: Color white;
		rowColorForEven:  Color veryLightGray muchLighter odd: Color white .
	^ treeMorph 