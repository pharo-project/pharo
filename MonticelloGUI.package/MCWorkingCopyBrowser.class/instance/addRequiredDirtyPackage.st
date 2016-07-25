actions
addRequiredDirtyPackage
	| dirtyPackages |
	
	dirtyPackages := self dirtyPackages.
	
	workingCopy ifNotNil: [:wc |
		dirtyPackages do: [:required |
			wc = required
				ifFalse: [ wc requirePackage: required package]]].
	
	self workingCopyListChanged