Extension { #name : #FileStream }

{ #category : #'*Tool-FileList' }
FileStream >> edit [
	"Create and schedule an editor on this file."

	Smalltalk tools fileList openEditorOn: self editString: nil.

]

{ #category : #'*Tool-FileList' }
FileStream class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>
	"Answer services for the given file"

	^ ((self isSourceFileSuffix: suffix) or: [ suffix = '*' ])
		ifTrue:
			[{self serviceRemoveLineFeeds.
			self serviceFileIn}]
		ifFalse:
			[#()]
]

{ #category : #'*Tool-FileList' }
FileStream class >> serviceFileIn [
	"Answer a service for filing in an entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'FileIn entire file'
		selector: #fileIn:
		description: 'File in the entire contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'Filein'
]

{ #category : #'*Tool-FileList' }
FileStream class >> serviceRemoveLineFeeds [
	"Answer a service for removing linefeeds from a file"

	^ FileModifyingSimpleServiceEntry
		provider: self 
		label: 'Remove line feeds'
		selector: #removeLineFeeds:	
		description: 'Remove line feeds in file'
		buttonLabel: 'Remove lfs'
]

{ #category : #'*Tool-FileList' }
FileStream class >> services [

	^ Array 
			with: self serviceRemoveLineFeeds
			with: self serviceFileIn
	
]
