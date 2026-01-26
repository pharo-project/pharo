Extension { #name : 'MCVersionReader' }

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> fileReaderServicesForFile: fullName suffix: suffix [

	<fileService>
	^ self allSubclasses flatCollect: [ :class |
			  (class isAbstract not and: [ suffix = class extension or: [ suffix = '*' ] ])
				  ifTrue: [ class services ]
				  ifFalse: [ #(  ) ] ]
]

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> loadVersionStream: stream fromDirectory: directory [
	| version repository |
	repository := MCDirectoryRepository new directory: directory.
	version := self versionFromStream: stream.
	version workingCopy repositoryGroup addRepository: repository.
	version load.

]

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> mergeVersionStream: stream [
	(self versionFromStream: stream) merge
]

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> openVersionFromStream: stream [
	(self versionFromStream: stream) open
]

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> serviceLoadVersion [
	^ (FileServiceEntry
		provider: self
		label: 'Load version'
		selector: #loadVersionStream:fromDirectory:
		description: 'Load a package version'
		buttonLabel: 'Load')
		argumentGetter: [ :fileList | { fileList binaryReadStream . fileList parent } ]
]

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> serviceMergeVersion [
	^ (FileServiceEntry
		provider: self
		label: 'Merge version'
		selector: #mergeVersionStream:
		description: 'Merge a package version into the image'
		buttonLabel: 'Merge')
		argumentGetter: [ :fileList | fileList binaryReadStream ]
]

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> serviceOpenVersion [
	^ (FileServiceEntry
		provider: self
		label: 'Open version'
		selector: #openVersionFromStream:
		description: 'Open a package version'
		buttonLabel: 'Open')
		argumentGetter: [ :fileList | fileList binaryReadStream ]
]

{ #category : '*MonticelloFileServices' }
MCVersionReader class >> services [
	^ Array 
		with: self serviceLoadVersion
		with: self serviceMergeVersion
		with: self serviceOpenVersion
]
