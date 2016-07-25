menus behavior
removeGroups
	self okToChange ifFalse: [ ^ self ].
	
	super removeGroups.
	self updatePackageView.