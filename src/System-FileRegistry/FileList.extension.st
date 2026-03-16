Extension { #name : 'FileList' }

{ #category : '*System-FileRegistry' }
FileList class >> fileInto: fullName [
	"File in all of the contents of the currently selected file"
	
	fullName ifNil: [ ^ self ].
	fullName asFileReference readStreamDo: [ :readStream |
		CodeImporter evaluateReadStream: readStream ].
	self inform: fullName asString , ' was filed in'.
]

{ #category : '*System-FileRegistry' }
FileList class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	^ (self sourceFileSuffixes includes: suffix)
		ifTrue: [ self services ]
		ifFalse: [ #() ]
]

{ #category : '*System-FileRegistry' }
FileList class >> serviceFileIn [
	"Answer a service for installing a file into a new change set"

	^ SimpleServiceEntry
		provider: self
		label: 'Install into the image'
		selector: #fileInto:
		description: 'Install the file as a body of code in the image: file-in the selected file into it'
		buttonLabel: 'Install'
]

{ #category : '*System-FileRegistry' }
FileList class >> services [
	"Answer potential file services associated with this class"

	^ { 
		self serviceFileIn
		}
]

{ #category : '*System-FileRegistry' }
FileList class >> sourceFileSuffixes [

	^#('st' 'cs')
]
