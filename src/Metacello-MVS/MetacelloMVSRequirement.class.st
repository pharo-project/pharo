"
I am one requirement the walk met, from what was asked to what it resolved to. I have the core MetacelloProjectRequest (who asked, the project spec as written in the baseline) and add what only the MVS walk knows:

request: the MetacelloProjectRequest; it goes to the resolved project when the asking candidate is selected.
from: the candidate that asked (its version's baseline declared me); superseded candidates ask too.
reference: the project spec resolution used, after spelling and replace; nil when it never got that far.
repository: the repository the reference names.
minimum: the version the reference asks at least.
resolved: the candidate my repository ended up with once the resolution is over (maybe higher than asked); nil when I failed.
findings: what resolving me found.
"
Class {
	#name : 'MetacelloMVSRequirement',
	#superclass : 'Object',
	#instVars : [
		'from',
		'reference',
		'resolved',
		'repository',
		'minimum',
		'request',
		'findings'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'instance creation' }
MetacelloMVSRequirement class >> from: aCandidate asked: aReference [

	^ self new setFrom: aCandidate asked: aReference
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> asked [

	^ request projectSpec
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> findings [

	^ findings
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> from [

	^ from
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> hasErrors [

	^ findings anySatisfy: [ :each | each isError ]
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> hasWarnings [

	^ findings anySatisfy: [ :each | each isWarning ]
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> isRaised [
	"Whether my repository ended up with a higher version than the minimum asked"

	^ resolved isNotNil and: [ minimum isNotNil and: [ minimum < resolved version ] ]
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> minimum [
	"The lowest version asked, once the resolver read it; nil for a branch or no version"

	^ minimum
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> minimum: aVersion [

	minimum := aVersion
]

{ #category : 'printing' }
MetacelloMVSRequirement >> printOn: aStream [
	"The candidate that asked, the repository asked as written, and the groups asked"

	aStream
		print: from;
		nextPutAll: ' asks ';
		nextPutAll: (self asked repositoryDescriptions ifEmpty: [ '?' ] ifNotEmpty: [ :descriptions | descriptions first ]).
	request loads ifNotEmpty: [ :names |
			aStream
				space;
				print: names asArray ]
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> reference [
	"The reference resolution used, after spelling and replace; nil when it never got that far"

	^ reference
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> reference: aReference [

	reference := aReference
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> repository [
	"The repository it ends up asking for (type://owner/name); nil when it never got that far"

	^ repository
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> repository: aRepository [

	repository := aRepository
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> request [

	^ request
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> resolveIn: aBuildList [
	"The resolution is over: remember what my repository ended up with"

	resolved := self hasErrors ifFalse: [ aBuildList at: self repository ifAbsent: [ nil ] ]
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> resolved [
	"The selection my repository ended up with, maybe higher than asked; nil when I failed or the resolution is not over"

	^ resolved
]

{ #category : 'accessing' }
MetacelloMVSRequirement >> setFrom: aCandidate asked: aProjectSpec [

	from := aCandidate.
	request := MetacelloProjectRequest requester: aCandidate resolvedProject projectSpec: aProjectSpec.
	findings := OrderedCollection new
]
