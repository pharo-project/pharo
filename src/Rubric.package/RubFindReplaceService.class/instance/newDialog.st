user-interface
newDialog
	| d |
	d := RubFindReplaceDialogWindow new model: self.
	d findPolicyChanged.
	d announcer when: MorphDeleted send: #whenDialogDeleted to: self.
	^ d
	