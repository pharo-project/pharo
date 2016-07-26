This is a TitleMorph used by MenuMorph.

"
	Example usage:

	| menu |
	
	menu := MenuMorph new.

	menu buildTitle: [ :titleMorph | titleMorph
		title: 'aCoolTitle';
		icon: self theme icons alertIcon;
		withCloseBox;
		withPinBox;
		color: Color purple		
	].

	... " add items" ...
	
	menu popUpInWorld
