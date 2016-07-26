form-scripting
submit

	^ self addBrickBack: (
		self newButton
			text: 'Submit';
			when: #onClicked do: [ self submitForm ])