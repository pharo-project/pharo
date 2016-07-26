private
showPasswordButton

	^ (CheckboxMorph 
			on: self 
			selected: #showPassword
			changeSelected: #showPassword:)
			label: 'Show password';
			labelClickable: true;
			height: 25.