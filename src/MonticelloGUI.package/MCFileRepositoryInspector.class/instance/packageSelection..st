morphic ui
packageSelection: aNumber
	"Track a package change to have the ancestors list loaded."

	"selectedPackage is the selected package name :("

	selectedPackage := aNumber isZero
		ifFalse: [ (self packageList at: aNumber) asString ].
	selectedPackageAncestors := nil.
	self versionSelection: 0.
	self
		changed: #packageSelection;
		changed: #versionList