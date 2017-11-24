"
Command to browse a package in versionner
"
Class {
	#name : #MBBrowsePackageCommand,
	#superclass : #MBAbstractPackageCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #documentation }
MBBrowsePackageCommand >> documentation [
	^ 'Open a system browser on the selected project configuration or package.'
]

{ #category : #execute }
MBBrowsePackageCommand >> execute [
	target classForBrowsing browse
]

{ #category : #accessing }
MBBrowsePackageCommand >> title [
	^ 'browse'
]
