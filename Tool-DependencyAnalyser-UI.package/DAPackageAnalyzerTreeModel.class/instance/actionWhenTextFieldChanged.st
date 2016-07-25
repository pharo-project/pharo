protocol
actionWhenTextFieldChanged
	textPackageField
		whenTextChanged: [ 
			textPackageField text 
				ifEmpty: [ self resetFilter ]
				ifNotEmpty: [ self filter: textPackageField text ].
			self buildRoots ]