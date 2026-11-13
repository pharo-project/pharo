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
		'request'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSCandidateVersion class >> resolvedProject: aResolvedProject repository: aRepository path: aPathOrNil [
	"A candidate for aResolvedProject, from aRepository (type://owner/name), its code at aPathOrNil inside it. Give it its version, and who required it"

	^ self new setResolvedProject: aResolvedProject repository: aRepository path: aPathOrNil
]

{ #category : 'testing' }
MetacelloMVSCandidateVersion >> appliesReplaces [
	"Only the replaces of a root apply"

	^ false
]

{ #category : 'pins' }
MetacelloMVSCandidateVersion >> beLocked [

	resolvedProject beLocked
]

{ #category : 'printing' }
MetacelloMVSCandidateVersion >> chainDisplayNames [
	"For people, the versions from the root down to me (see chainLine)"

	^ requiredBy chainDisplayNames
		  addLast: (self isInstalled
				   ifTrue: [ 'installed ' , self displayName ]
				   ifFalse: [ self displayName ]);
		  yourself
]

{ #category : 'printing' }
MetacelloMVSCandidateVersion >> chainLine [
	"From the root down to me, on one line, with project names: Moose v13.0.0 > Famix v1.2.1"

	^ ' > ' join: self chainDisplayNames
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> chainLines [
	"One line per version, from the root down to me"

	^ requiredBy chainLines
		  addLast: self line;
		  yourself
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> chainString [
	"One line per version, from the root down to me"

	^ String lf join: self chainLines
]

{ #category : 'printing' }
MetacelloMVSCandidateVersion >> displayName [
	"For people: my project and version (Famix v1.2.1)"

	^ self project , ' ' , version string
]

{ #category : 'findings' }
MetacelloMVSCandidateVersion >> isAboutRepository: aRepository [

	^ repository = aRepository
]

{ #category : 'findings' }
MetacelloMVSCandidateVersion >> isAboutSameRepositoryAs: aSubject [

	^ aSubject isAboutRepository: repository
]

{ #category : 'testing' }
MetacelloMVSCandidateVersion >> isInstalled [
	"Whether the image already has me: the image required me"

	^ requiredBy requiresInstalledVersions
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

{ #category : 'findings' }
MetacelloMVSCandidateVersion >> isSelectedIn: aResolution [
	"Whether I am the version aResolution selected for my repository"

	^ aResolution buildList at: repository ifPresent: [ :selected | selected == self ] ifAbsent: [ false ]
]

{ #category : 'findings' }
MetacelloMVSCandidateVersion >> isSupersededIn: aResolution [

	^ aResolution isSupersededSelection: self
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

{ #category : 'testing' }
MetacelloMVSCandidateVersion >> mustReplace [
	"Whether the replaces of my baseline apply: only when the user required me"

	^ requiredBy appliesReplaces
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

{ #category : 'printing' }
MetacelloMVSCandidateVersion >> printRequiredByOn: aStream [
	"Who required me, on the line after the text of a finding about me"

	aStream
		lf;
		nextPutAll: 'Required by: ';
		nextPutAll: self chainLine
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

{ #category : 'requests' }
MetacelloMVSCandidateVersion >> requestFor: aProjectSpec [
	"My request for the project aProjectSpec names, which my baseline declares"

	^ MetacelloProjectRequest requester: resolvedProject projectSpec: aProjectSpec
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> requiredBy [
	"Who required me: another candidate version, the user (MetacelloUserRequest) for a root, or the image (MetacelloImageRequest) for what it has installed"

	^ requiredBy
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> requiredBy: aRequester [

	requiredBy := aRequester
]

{ #category : 'testing' }
MetacelloMVSCandidateVersion >> requiresInstalledVersions [

	^ false
]

{ #category : 'accessing' }
MetacelloMVSCandidateVersion >> resolvedProject [
	"My project: the spec asked, at my version once its baseline is read"

	^ resolvedProject
]

{ #category : 'initialization' }
MetacelloMVSCandidateVersion >> setResolvedProject: aResolvedProject repository: aRepository path: aPathOrNil [

	resolvedProject := aResolvedProject.
	repository := aRepository.
	path := aPathOrNil
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
