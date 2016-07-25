instance creation
newInspectButtonFor: anAssociation

	^ self newActionButton
		hintTitle: 'Inspect';
		hintText: 'will open inspector window';
		icon: self themer bindingThemer inspectIcon;
		action: [ : b | self remove. self announce: #inspect event: anAssociation. ]