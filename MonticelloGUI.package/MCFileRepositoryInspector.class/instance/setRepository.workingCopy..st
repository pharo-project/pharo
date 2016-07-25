accessing
setRepository: aFileBasedRepository workingCopy: aWorkingCopy
	order := self class order.
	repository := aFileBasedRepository.
	
	aWorkingCopy
		ifNil: [ selectedPackage := self packageList isEmpty ifFalse: [self packageList first asString]]
		ifNotNil: [ selectedPackage := aWorkingCopy ancestry ancestorString copyUpToLast: $-.
			selectedPackage ifEmpty: [
				selectedPackage := aWorkingCopy package name ]]. 
	
	[ self refresh.
	MCWorkingCopy addDependent: self.] fork.
