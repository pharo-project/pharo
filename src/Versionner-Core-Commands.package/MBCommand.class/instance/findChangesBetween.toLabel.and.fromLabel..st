changes
findChangesBetween: versionSnapshot toLabel: toLabel and: ancestorVersionSnapshot fromLabel: fromLabel
	"View changes between 2 versions."
	| patch |
	
	patch := versionSnapshot patchRelativeToBase: ancestorVersionSnapshot.
	patch isNil ifTrue: [^ self].
	patch isEmpty
		ifTrue: [
			self uiManager inform: 'No changes' ]
		ifFalse: [
			self
				viewChanges: patch
				from: fromLabel
				to: toLabel
				title: 'Changes between ', fromLabel, ' and ', toLabel   ]