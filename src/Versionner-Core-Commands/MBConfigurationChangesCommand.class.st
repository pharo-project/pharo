"
Command to browse changes on a configuration in versionner
"
Class {
	#name : #MBConfigurationChangesCommand,
	#superclass : #MBConfigurationInfoCommand,
	#category : 'Versionner-Core-Commands'
}

{ #category : #documentation }
MBConfigurationChangesCommand >> documentation [
	^ 'View changes for selected configuration against version in selected repository.'
]

{ #category : #execute }
MBConfigurationChangesCommand >> execute [
	"View the changes made in the working copy."
	
	| wc repo |
	wc := target workingCopy.
	(repo := self selectRepositoryFromWorkingCopy: wc) ifNil: [ ^self ].
	self findChangesFor: wc relativeToRepository: repo
]

{ #category : #accessing }
MBConfigurationChangesCommand >> title [
	^ 'changes'
]
