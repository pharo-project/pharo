morphic ui
packageSelection: aNumber
	selectedPackage := aNumber isZero ifFalse: [ packages at: aNumber ].
	versions := repository versionsAvailableForPackage: selectedPackage.
	self changed: #packageSelection; changed: #versionList