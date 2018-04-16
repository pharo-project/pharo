"
Writer for Metacello MCM
"
Class {
	#name : #MCMcmWriter,
	#superclass : #MCWriter,
	#category : #MonticelloConfigurations
}

{ #category : #writing }
MCMcmWriter class >> fileOut: aConfiguration on: aStream [
	| inst |
	inst := self on: aStream.
	inst writeConfiguration: aConfiguration.
	inst close.
	

]

{ #category : #accessing }
MCMcmWriter class >> readerClass [
	^ MCMcmReader
]

{ #category : #writing }
MCMcmWriter >> close [
	stream close
]

{ #category : #writing }
MCMcmWriter >> writeConfiguration: aConfiguration [

	stream nextPut: $(.

	aConfiguration repositories do: [:ea | 
		stream newLine.
		stream nextPutAll: 'repository '.
		(MCConfiguration repositoryToArray: ea) printElementsOn: stream].

	aConfiguration dependencies do: [:ea | 
		stream newLine.
		stream nextPutAll: 'dependency '.
		(MCConfiguration dependencyToArray: ea) printElementsOn: stream].

	stream newLine.
	stream nextPut: $).
	stream newLine.
]
