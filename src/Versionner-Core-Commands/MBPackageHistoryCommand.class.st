"
Command to browse package history in versionner
"
Class {
	#name : #MBPackageHistoryCommand,
	#superclass : #MBAbstractPackageCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #documentation }
MBPackageHistoryCommand >> documentation [
	^ 'view package history'
]

{ #category : #execute }
MBPackageHistoryCommand >> execute [

	self browseVersionHistory
]

{ #category : #accessing }
MBPackageHistoryCommand >> title [
	^ 'history'
]
