callbacks
elementColumn
	| rowElements |
	
	rowElements := OrderedCollection new
		add: self displayText;
		addAll: (self tagMorphsFrom: self containerTree glamourPresentation);
		yourself.
	
	^ (UITheme current newRowIn: World for: rowElements).