"
I am one candidate version of a repository the MVS walk reached: a MetacelloResolvedProject (the project spec asked, its baseline read later if I am selected), my repository (type://owner/name), the path of the code in it, my version, and the candidate that required me (nil for the root, and for what the image already has: then my origin is 'installed').

A project installed from a branch has a branch and a placeholder version; one locked in the registry is locked. Both are pins, kept by my resolved project: nothing the resolution asks changes them. A candidate that loses to a higher version of its repository is superseded: it stays in the report, for debugging.
"
Class {
	#name : 'MetacelloMVSCandidate',
	#superclass : 'Object',
	#instVars : [
		'resolvedProject',
		'repository',
		'path',
		'version',
		'requiredBy',
		'request',
		'installed'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSCandidate class >> resolvedProject: aResolvedProject repository: aRepository path: aPathOrNil [
	"A candidate for aResolvedProject, from aRepository (type://owner/name), its code at aPathOrNil inside it. Give it its version, and who required it"

	^ self new setResolvedProject: aResolvedProject repository: aRepository path: aPathOrNil
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> beInstalled [
	"The image already has me"

	installed := true
]

{ #category : 'pins' }
MetacelloMVSCandidate >> beLocked [

	resolvedProject beLocked
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> chainString [
	"One line per selection, from the root down to me"

	requiredBy ifNil: [ ^ self line ].
	^ requiredBy chainString , String lf , self line
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> isInstalled [
	"Whether the image already has me"

	^ installed
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> isLocked [

	^ resolvedProject isLocked
]

{ #category : 'testing' }
MetacelloMVSCandidate >> isSameVersionAs: aCandidate [
	"Whether aCandidate is my project at my version (each requirement makes its own candidate)"

	^ self repository = aCandidate repository and: [ self version = aCandidate version ]
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> line [

	^ String streamContents: [ :stream |
			  stream nextPutAll: '  '.
			  self isInstalled ifTrue: [ stream nextPutAll: 'installed ' ].
			  stream
				  nextPutAll: self repository;
				  space;
				  nextPutAll: version string ]
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> path [
	"The path of my code inside my repository, or nil"

	^ path
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> printOn: aStream [

	aStream
		nextPutAll: self repository;
		space;
		nextPutAll: version string
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> project [
	"The name of my project: its baseline's"
	^ resolvedProject projectSpec className allButFirst: 'BaselineOf' size
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> repository [
	"type://owner/name"

	^ repository
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> repositoryDescription [
	"The Metacello repository description to load me from: my repository at my version, with my path"

	^ String streamContents: [ :stream |
			  stream
				  nextPutAll: repository;
				  nextPut: $:;
				  nextPutAll: version string.
			  path ifNotNil: [ :aPath |
					  stream
						  nextPut: $/;
						  nextPutAll: aPath ] ]
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> request [
	"The MetacelloProjectRequest I was chosen for: findings about me that no requirement holds are about it"

	^ request
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> request: aProjectRequestOrNil [

	request := aProjectRequestOrNil
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> requiredBy [
	"The candidate that required me; nil for the root and for what the image has"

	^ requiredBy
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> requiredBy: aCandidateOrNil [

	requiredBy := aCandidateOrNil
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> resolvedProject [
	"My project: the spec asked, at my version once its baseline is read"

	^ resolvedProject
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> setResolvedProject: aResolvedProject repository: aRepository path: aPathOrNil [

	resolvedProject := aResolvedProject.
	repository := aRepository.
	path := aPathOrNil.
	installed := false
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> spec [
	"The project spec asked"

	^ resolvedProject projectSpec
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> version [

	^ version
]

{ #category : 'accessing' }
MetacelloMVSCandidate >> version: aVersion [

	version := aVersion
]
