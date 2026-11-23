"
I am one request a resolver met: a project asked by another, as its baseline declares it.
- requester: the MetacelloResolvedProject that asked, nil for a root of the load;
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
MetacelloProjectRequest class >> requester: aRequester projectSpec: aProjectSpec [

	^ self new setRequester: aRequester projectSpec: aProjectSpec
]

{ #category : 'accessing' }
MetacelloProjectRequest >> addFinding: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'accessing' }
MetacelloProjectRequest >> chain [
	"The names of the projects that led to this request, from the root"

	^ requester chainNames
]

{ #category : 'accessing' }
MetacelloProjectRequest >> findings [

	^ findings
]

{ #category : 'accessing' }
MetacelloProjectRequest >> loads [
	"The groups asked; empty for the default ones"
	^ projectSpec loads
]

{ #category : 'printing' }
MetacelloProjectRequest >> printOn: aStream [

	aStream
		nextPutAll: self requesterName;
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
	"Who asked: a resolved project, the user (MetacelloUserRequest) for a root, or the image (MetacelloImageRequest)"

	^ requester
]

{ #category : 'accessing' }
MetacelloProjectRequest >> requesterName [

	^ requester name
]

{ #category : 'accessing' }
MetacelloProjectRequest >> setRequester: aRequester projectSpec: aProjectSpec [

	requester := aRequester.
	projectSpec := aProjectSpec.
	findings := OrderedCollection new
]
