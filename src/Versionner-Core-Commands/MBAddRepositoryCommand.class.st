"
Command to add a repository in Versionner
"
Class {
	#name : #MBAddRepositoryCommand,
	#superclass : #MBDevCycleCommands,
	#category : #Versionner-Core-Commands
}

{ #category : #documentation }
MBAddRepositoryCommand >> documentation [
	^ 'Add a Monticello repository to the default repository group.'
]

{ #category : #execute }
MBAddRepositoryCommand >> execute [

	MCTool newRepository ifNotNil:
		[:repos | MCRepositoryGroup default addRepository: repos ].


	
]

{ #category : #accessing }
MBAddRepositoryCommand >> title [
	^ 'add repository'
]
