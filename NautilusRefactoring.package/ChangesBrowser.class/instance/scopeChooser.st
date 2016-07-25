accessing
scopeChooser
	^ scopeChooser
		ifNil: [ 
			scopeChooser := self instantiate: EnvironmentChooser.
			scopeChooser
				whenEnvironmentChanged: [ self updateChanges ];
				label: 'Scope:'.
			scopeChooser ]