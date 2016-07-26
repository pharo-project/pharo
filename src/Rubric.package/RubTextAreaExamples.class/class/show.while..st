private
show: aMessage while: aBlock
	| m |
	m := RubEditingArea new
		backgroundColor: Color black;
		beReadOnly;
		beWrapped;
		width: 300;
		setTextWith: aMessage;
		font: (LogicalFont familyName: 'Arial' pointSize: 20);
		textColor: Color white;
		openInWorld;
		centered;
		yourself.
	
	[ aBlock ensure: [ m delete ] ] fork.