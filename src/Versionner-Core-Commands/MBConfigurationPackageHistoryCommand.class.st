"
Command to browse package history for configurations in versionner
"
Class {
	#name : #MBConfigurationPackageHistoryCommand,
	#superclass : #MBConfigurationInfoCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #documentation }
MBConfigurationPackageHistoryCommand >> documentation [
	^ 'view package history'
]

{ #category : #execute }
MBConfigurationPackageHistoryCommand >> execute [
	self browseVersionHistory
]

{ #category : #accessing }
MBConfigurationPackageHistoryCommand >> title [
	^ 'history'
]
