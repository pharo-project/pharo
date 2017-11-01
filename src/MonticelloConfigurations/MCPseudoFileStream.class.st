"
A pseudo file stream which can be used for updates.
"
Class {
	#name : #MCPseudoFileStream,
	#superclass : #RWBinaryOrTextStream,
	#instVars : [
		'localName'
	],
	#category : 'MonticelloConfigurations'
}

{ #category : #accessing }
MCPseudoFileStream >> localName [
	^localName
]

{ #category : #accessing }
MCPseudoFileStream >> localName: aString [
	localName := aString
]
