actions
addRequiredPackage
	| chosen |
	workingCopy
		ifNotNil: [ :wc | 
			chosen := (SearchFacade mcPackageSearchRejectAll: [ :ea | ea = wc or: [ wc requiredPackages includes: ea package ] ])
				chooseFromOwner: self window.
			chosen
				ifNotNil: [ 
					wc requirePackage: chosen package.
					self workingCopyListChanged ] ]