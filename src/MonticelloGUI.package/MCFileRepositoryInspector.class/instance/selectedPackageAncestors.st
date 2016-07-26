accessing
selectedPackageAncestors
	selectedPackageAncestors
		ifNil: [ 
			selectedPackage
				ifNotNil: [ 
					(MCWorkingCopy registry at: (MCPackage named: selectedPackage) ifAbsent: [  ])
						ifNotNil: [ :aWorkingCopy | selectedPackageAncestors := aWorkingCopy ancestry breadthFirstAncestors collect: #name ] ] ].
	^ selectedPackageAncestors