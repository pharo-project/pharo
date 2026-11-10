"
I am one request a resolver met: a project asked by another, as its baseline declares it.

- requester: the MetacelloResolvedProject that asked, nil for the root of the load (or for a version spec resolved on its own);
- projectSpec: the project spec asked: its repository (and so its version) and its loads (the groups asked, empty for the default ones);
- findings: the MetacelloResolutionFindings about me.

Following requesters gives the chain that led to a project (see chain).
"
Class {
	#name : 'MetacelloProjectRequest',
	#superclass : 'Object',
	#instVars : [
		'requester',
		'projectSpec',
		'findings'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'instance creation' }
MetacelloProjectRequest class >> requester: aResolvedProjectOrNil projectSpec: aProjectSpec [

	^ self new setRequester: aResolvedProjectOrNil projectSpec: aProjectSpec
]

{ #category : 'accessing' }
MetacelloProjectRequest >> addFinding: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'accessing' }
MetacelloProjectRequest >> chain [
	"The names of the projects that led to this request, from the root"

	| names project |
	names := OrderedCollection new.
	project := requester.
	[ project isNotNil ] whileTrue: [
			names addFirst: project name.
			project := project requests ifEmpty: [ nil ] ifNotEmpty: [ :requests | requests first requester ] ].
	^ names
]

{ #category : 'accessing' }
MetacelloProjectRequest >> findings [

	^ findings
]

{ #category : 'accessing' }
MetacelloProjectRequest >> loads [
	"The groups asked; empty for the default ones"

	^ projectSpec loads ifNil: [ #(  ) ]
]

{ #category : 'accessing' }
MetacelloProjectRequest >> printOn: aStream [

	aStream
		nextPutAll: (self requesterName ifNil: [ 'the load' ]);
		nextPutAll: ' asks ';
		nextPutAll: self projectName.
	self loads ifNotEmpty: [ :names |
			aStream
				nextPutAll: ' ';
				print: names asArray ]
]

{ #category : 'accessing' }
MetacelloProjectRequest >> projectName [

	^ projectSpec name
]

{ #category : 'accessing' }
MetacelloProjectRequest >> projectSpec [
	"The project spec asked, as declared"

	^ projectSpec
]

{ #category : 'accessing' }
MetacelloProjectRequest >> requester [
	"The resolved project that asked; nil for the root"

	^ requester
]

{ #category : 'accessing' }
MetacelloProjectRequest >> requesterName [

	^ requester ifNotNil: [ :project | project name ]
]

{ #category : 'accessing' }
MetacelloProjectRequest >> setRequester: aResolvedProjectOrNil projectSpec: aProjectSpec [

	requester := aResolvedProjectOrNil.
	projectSpec := aProjectSpec.
	findings := OrderedCollection new
]
