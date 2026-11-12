"
I am one candidate version of a repository the MVS walk reached: a MetacelloResolvedProject (the project spec asked, its baseline read later if I am selected), my repository (type://owner/name), the path of the code in it, my version, and the candidate that required me (nil for the root, and for what the image already has: then my origin is 'installed').

A project installed from a branch has a branch and a placeholder version; one locked in the registry is locked. Both are pins, kept by my resolved project: nothing the resolution asks changes them. A candidate that loses to a higher version of its repository is superseded: it stays in the report, for debugging.
"
Class {
	#name : 'MetacelloMVSCandidateVersion',
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
MetacelloMVSCandidateVersion class >> resolvedProject: aResolvedProject repository: aRepository path: aPathOrNil [
	"A candidate for aResolvedProject, from aRepository (type://owner/name), its code at aPathOrNil inside it. Give it its version, and who required it"

	^ self new setResolvedProject: aResolvedProject repository: aRepository path: aPathOrNil
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> beInstalled [
	"The image already has me"

	installed := true
]

{ #category : 'pins' }
MetacelloMVSCandidateVersion >> beLocked [

	resolvedProject beLocked
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> chainString [
	"One line per selection, from the root down to me"

	requiredBy ifNil: [ ^ self line ].
	^ requiredBy chainString , String lf , self line
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> isInstalled [
	"Whether the image already has me"

	^ installed
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> isLocked [

	^ resolvedProject isLocked
]

{ #category : 'testing' }
MetacelloMVSCandidateVersion >> isSameVersionAs: aCandidate [
	"Whether aCandidate is my project at my version (each requirement makes its own candidate)"

	^ self repository = aCandidate repository and: [ self version = aCandidate version ]
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> line [

	^ String streamContents: [ :stream |
			  stream nextPutAll: '  '.
			  self isInstalled ifTrue: [ stream nextPutAll: 'installed ' ].
			  stream
				  nextPutAll: self repository;
				  space;
				  nextPutAll: version string ]
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> path [
	"The path of my code inside my repository, or nil"

	^ path
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> printOn: aStream [

	aStream
		nextPutAll: self repository;
		space;
		nextPutAll: version string
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> project [
	"The name of my project: its baseline's"
	^ resolvedProject projectSpec className allButFirst: 'BaselineOf' size
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> repository [
	"type://owner/name"

	^ repository
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> repositoryDescription [
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
MetacelloMVSCandidateVersion >> request [
	"The MetacelloProjectRequest I was chosen for: findings about me that no requirement holds are about it"

	^ request
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> request: aProjectRequestOrNil [

	request := aProjectRequestOrNil
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> requiredBy [
	"The candidate that required me; nil for the root and for what the image has"

	^ requiredBy
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> requiredBy: aCandidateOrNil [

	requiredBy := aCandidateOrNil
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> resolvedProject [
	"My project: the spec asked, at my version once its baseline is read"

	^ resolvedProject
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> setResolvedProject: aResolvedProject repository: aRepository path: aPathOrNil [

	resolvedProject := aResolvedProject.
	repository := aRepository.
	path := aPathOrNil.
	installed := false
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> spec [
	"The project spec asked"

	^ resolvedProject projectSpec
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> version [

	^ version
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> version: aVersion [

	version := aVersion
]
