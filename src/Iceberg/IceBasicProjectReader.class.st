"
I am a basic project reader that is used by default in case no other project reader was found.
I create an IceBasicProject in case a .project file is found, or an IceUnbornProject otherwise.
"
Class {
	#name : #IceBasicProjectReader,
	#superclass : #IceProjectReader,
	#category : #'Iceberg-Project'
}

{ #category : #testing }
IceBasicProjectReader >> canReadProjectFromCommitish: aCommitish [
	
	^ (aCommitish fileSystem / '.project') exists
]

{ #category : #reading }
IceBasicProjectReader >> readProjectFromCommitish: anIceCommitish [ 

	| properties projectFileReference |
	projectFileReference := anIceCommitish fileSystem / '.project'.
	properties := projectFileReference binaryReadStreamDo: [ :stream | STON fromStream: (ZnCharacterReadStream on: stream encoding: 'utf8') ].
	^ IceBasicProject onProperties: properties commitish: anIceCommitish
]
