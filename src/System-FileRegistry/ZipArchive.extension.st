Extension { #name : #ZipArchive }

{ #category : #'*System-FileRegistry' }
ZipArchive class >> fileReaderServicesForFile: fullName suffix: suffix [ 
	<fileService>
	"Answers a collection of services that can handle the file and suffixes"
	|  services |
	services := OrderedCollection new.
	({'zip'.'sar'.'pr'. 'mcz'. '*'} includes: suffix)
		ifTrue: [	services add: self serviceExtractAll].
	^services.
]

{ #category : #'*System-FileRegistry' }
ZipArchive class >> serviceExtractAll [
	"Answer a service for extracting files in a zip to a user-specified directory"

	^ FileModifyingSimpleServiceEntry 
		provider: self
		label: 'extract all to...'
		selector: #extractAllIn: 
		description: 'extract all files to a user-specified directory'
		buttonLabel: 'extract all'
]
