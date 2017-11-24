"
I implement a service for browsing Epicea logs.
"
Class {
	#name : #EpOmbuFileReaderService,
	#superclass : #Object,
	#category : #'EpiceaBrowsers-Integration'
}

{ #category : #private }
EpOmbuFileReaderService class >> browseLogInFileNamed: fileName [
	(EpLog freshFromFile: fileName asFileReference) browseEvents
]

{ #category : #'System-FileRegistry' }
EpOmbuFileReaderService class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	(fullName endsWith: self fileSuffix)
		ifFalse: [ ^ #() ].

	^ { self service }
]

{ #category : #private }
EpOmbuFileReaderService class >> fileSuffix [
	^ OmFileStore defaultFileSuffix
]

{ #category : #'System-FileRegistry' }
EpOmbuFileReaderService class >> service [
	^ SimpleServiceEntry 
		provider: self 
		label: 'Browse Epicea Log'
		selector: #browseLogInFileNamed:
		description: 'Browse the Epicea log.'
		buttonLabel: 'Browse Epicea Log'
]
