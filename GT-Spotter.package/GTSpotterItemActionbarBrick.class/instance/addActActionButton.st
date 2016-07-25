adding
addActActionButton

	actOnButton := (self newActionButtonBrick
			icon: self themer spotterThemer iconAct;
			hintTitle: 'Default action';
			hintText: '<enter>';
			action: [ self owner beSelected. self stepModel actDueTo: #actionButton ];
			yourself).

	self addActionButton: actOnButton.