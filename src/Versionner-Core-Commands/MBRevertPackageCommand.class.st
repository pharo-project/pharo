"
Command to revert a package in versionner
"
Class {
	#name : #MBRevertPackageCommand,
	#superclass : #MBAbstractPackageCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #documentation }
MBRevertPackageCommand >> documentation [
	^ 'Revert the changes that have been made to this package.'
]

{ #category : #execute }
MBRevertPackageCommand >> execute [

	self pickAncestorVersionInfo ifNotNil: [:info |
		(target workingCopy repositoryGroup versionWithInfo: info
			ifNone: [^self inform: 'No repository found for ', info name]
		) load].

]

{ #category : #private }
MBRevertPackageCommand >> pickAncestorVersionInfo [
	| ancestors |
	ancestors := target workingCopy ancestry breadthFirstAncestors.
	^self uiManager 
		chooseFrom: (ancestors collect: [:ea | ea name])
		values: ancestors
		title: 'Package:' 

]

{ #category : #accessing }
MBRevertPackageCommand >> title [
	^ 'revert'
]
