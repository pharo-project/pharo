instance creation
newPageRow: playPage in: aGTPlayBook

	^ self newRow
		addBrickBack: (self newPageLabel
			textColor: self themer textColor;
			text: (playPage content asString contractTo: 50));
		addBrickBack: (self newAcceptButtonFor: playPage);
		when: #onDoubleClicked do: [ self remove. self announce: #pageAccepted event: playPage ];
		with: [ :aRow | aRow 	
			when: #onMouseHover do: [ self showActionsOf: aRow ];
			when: #onMouseUnhover do: [ self hideActionsOf: aRow ] ];
		yourself