construction-title
buildTitle: aBlock 
	
"
	Example usage:

	menu buildTitle: [ :titleMorph | titleMorph
		title: 'aCoolTitle';
		icon: self theme icons alertIcon;
		withCloseBox;
		withPinBox;
		color: Color purple		
	].
"
	
	titleMorph ifNil: [ self createTitleMorph ].
	
	aBlock value: titleMorph.