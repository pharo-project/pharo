"
I am an abstract class defining the interface for reading sources containing class and method definitions.
I should define loadDefinitions as a subclassResponsibility, but don't.
MCVersionReader could be a subclass of me for reading sources containing additional metadata, but isn't. 

I'm useless and can be safely removed. I'm only referenced in MCMczReader>>extractDefinitionsFrom: and it can be replaced by MCReader.
"
Class {
	#name : #MCSnapshotReader,
	#superclass : #MCReader,
	#instVars : [
		'definitions'
	],
	#category : 'Monticello-Storing'
}

{ #category : #accessing }
MCSnapshotReader class >> snapshotFromStream: aStream [
	^ (self on: aStream) snapshot
]

{ #category : #accessing }
MCSnapshotReader >> definitions [
	definitions ifNil: [self loadDefinitions].
	^ definitions
]

{ #category : #accessing }
MCSnapshotReader >> snapshot [
	^ MCSnapshot fromDefinitions: self definitions
]
