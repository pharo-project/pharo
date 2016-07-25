accessing
setRepository: aRepository workingCopy: aWorkingCopy
	repository := aRepository.
	aWorkingCopy ifNotNil: [ selectedPackage := aWorkingCopy package].
	self refresh