instance creation
newGoButtonFor: anAssociation

	^ self newActionButton
		hintTitle: 'Go';
		hintText: 'open new pane to the right';
		icon: self themer bindingThemer goIcon;
		action: [ : b | self remove. self announce: #go event: anAssociation. ]