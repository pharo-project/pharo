Extension { #name : 'ChangeSet' }

{ #category : '*System-Changes-FileServices' }
ChangeSet class >> fileIntoNewChangeSet: fullName [
	"File in all of the contents of the currently selected file, if any, into a new change set."
	fullName ifNil: [^ self ].
	fullName asFileReference readStreamDo: [ :readStream |
		"Use UUID to avoid name collision. I'm not sure way a name is needed nor why is sould be unique BTW"
		self newChangesFromStream: readStream named: fullName asFileReference basename withoutPeriodSuffix , '_' , UUID new asString ]
]

{ #category : '*System-Changes-FileServices' }
ChangeSet class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	^ (#('st' 'cs') includes: suffix)
		ifTrue: [ self services]
		ifFalse: [#()]
]

{ #category : '*System-Changes-FileServices' }
ChangeSet class >> serviceFileIntoNewChangeSet [
	"Answer a service for installing a file into a new change set"

	^ SimpleServiceEntry
		provider: self
		label: 'Install into the image'
		selector: #fileIntoNewChangeSet:
		description: 'Install the file as a body of code in the image: create a new change set and file-in the selected file into it'
		buttonLabel: 'Install'
]

{ #category : '*System-Changes-FileServices' }
ChangeSet class >> services [
	^ Array with: self serviceFileIntoNewChangeSet
]
