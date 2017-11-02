Extension { #name : #Object }

{ #category : #'*System-FileRegistry' }
Object class >> fileReaderServicesForDirectory: aFileDirectory [
	<directoryService>
	"Backstop"
	^#()
]

{ #category : #'*System-FileRegistry' }
Object class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>
	"Backstop"
	^#()
]
