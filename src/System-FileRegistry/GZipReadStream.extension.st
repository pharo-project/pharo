Extension { #name : #GZipReadStream }

{ #category : #'*System-FileRegistry' }
GZipReadStream class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>
	| services |
	suffix = 'gz' | (suffix = '*')
		ifFalse: [ ^ #() ].
	services := OrderedCollection new.
	suffix = '*' | (fullName asLowercase endsWith: '.cs.gz') | (fullName asLowercase endsWith: '.mcs.gz')
		ifTrue: [ 
			services add: self serviceFileIn.
			(Smalltalk globals includesKey: #ChangeSorter)
				ifTrue: [ services add: self serviceFileIntoNewChangeSet ] ].
	services addAll: self services.
	^ services
]

{ #category : #'*System-FileRegistry' }
GZipReadStream class >> serviceDecompressToFile [

	^ FileModifyingSimpleServiceEntry 
				provider: self 
				label: 'Decompress to file'
				selector: #saveContents:
				description: 'Decompress to file'
]

{ #category : #'*System-FileRegistry' }
GZipReadStream class >> serviceFileIn [
	"Answer a service for filing in an entire file"

	^ SimpleServiceEntry 
		provider: self 
		label: 'FileIn entire file'
		selector: #fileIn:
		description: 'File in the entire decompressed contents of the file, which is expected to contain Smalltalk code in fileout ("chunk") format'
		buttonLabel: 'Filein'


]

{ #category : #'*System-FileRegistry' }
GZipReadStream class >> serviceFileIntoNewChangeSet [
	"Answer a service for filing in an entire file"
	^ SimpleServiceEntry
		provider: self
		label: 'Install into new change set'
		selector: #fileIntoNewChangeSet:
		description: 'Install the decompressed contents of the file as a body of code in the image: create a new change set and file-in the selected file into it'
		buttonLabel: 'Install'
]

{ #category : #'*System-FileRegistry' }
GZipReadStream class >> serviceViewDecompress [

	^ SimpleServiceEntry 
				provider: self 
				label: 'View decompressed'
				selector: #viewContents:
				description: 'View decompressed' 

]

{ #category : #'*System-FileRegistry' }
GZipReadStream class >> services [

	^ Array 
		with: self serviceViewDecompress
		with: self serviceDecompressToFile
	
]
