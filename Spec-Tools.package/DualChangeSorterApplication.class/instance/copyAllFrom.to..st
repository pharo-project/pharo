menu - change set
copyAllFrom: src to: dest

	self checkThatSidesDiffer: [ ^ self ].
	self model copyAllChangesFrom: (src selectedChangeSet) to: (dest selectedChangeSet).
	dest updateClassesList.