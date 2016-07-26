instance creation
newRemoveButtonFor: anAssociation

	^ self newActionButton
		beError;
		hintTitle: 'Remove binding';
		hintText: 'will unassign variable';
		icon: self themer bindingThemer removeIcon;
		action: [ : b |
			b owner disable.
			GLMAnimation swipeLeft
				onCompleted: [
					self announce: #bindingRemoved event: anAssociation.
					self model origin remove: anAssociation.
					self invalidate ];
				animate: b owner ]