actions
installVersion: aVersionName
	| metacelloVersion |
	self inform: 'Installing ', self configurationName, ' ', aVersionName.
	
	metacelloVersion := (self metacelloVersion: aVersionName).
	
	self handleMergeConflictDuring: [
		(self hasOption: 'group')
			ifTrue: [ metacelloVersion load: self groups ]
			ifFalse: [ metacelloVersion load ]].