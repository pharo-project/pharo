instance creation
newBindingRow: anAssociation in: aCollection

	^ self newRow
		borderWidth: { 0 . 0 . aCollection last == anAssociation ifTrue: [ 0 ] ifFalse: [ self themer bindingThemer borderWidth ] . 0 };
		addAllBricks: {
			self newBindingLabel
				textColor: self themer textLightColor;
				text: anAssociation key.
			self newBindingLabel
				text: anAssociation value asString.
			self newInspectButtonFor: anAssociation.
			self newGoButtonFor: anAssociation.
			self newRemoveButtonFor: anAssociation };
		with: [ :aRow | aRow 	
			when: #onMouseHover do: [ self showActionsOf: aRow ];
			when: #onMouseUnhover do: [ self hideActionsOf: aRow ] ];
		yourself