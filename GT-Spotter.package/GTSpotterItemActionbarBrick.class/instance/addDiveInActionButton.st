adding
addDiveInActionButton
			
	(self stepModel canDiveIn: self candidate) ifTrue: [
		diveInButton := (self newActionButtonBrick
			icon: self themer spotterThemer iconDiveIn;
			hintTitle: 'Dive in';
			hintText: '<',OSPlatform current defaultModifier name asLowercase,'+right>';
			action: [ self owner beSelected. self stepModel diveInDueTo: #actionButton ];
			yourself).
		
		self addActionButton: diveInButton ]