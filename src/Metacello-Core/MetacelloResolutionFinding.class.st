"
I am something a resolver found: an error (the load cannot go ahead), a warning, or an info (worth knowing, it changes nothing: for example a newer version a baseline could ask for).

- severity: #error, #warning or #info;
- kind: a symbol naming what was found (#cannotResolve, #majorConflict...), for code and tests;
- text: what was found, for people;
- request: the MetacelloProjectRequest I am about, or the MetacelloUserRequest for the load as a whole; its chain tells how the load got there. I am added to its findings when created.

A report sorts and groups findings as it needs (see MetacelloResolution>>errors, warnings, infos).
"
Class {
	#name : 'MetacelloResolutionFinding',
	#superclass : 'Object',
	#instVars : [
		'severity',
		'kind',
		'text',
		'request'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'instance creation' }
MetacelloResolutionFinding class >> error: aKind text: aString request: aRequest [

	^ self
		  severity: #error
		  kind: aKind
		  text: aString
		  request: aRequest
]

{ #category : 'instance creation' }
MetacelloResolutionFinding class >> info: aKind text: aString request: aRequest [
	"Something worth knowing that changes nothing, such as a newer version a baseline could ask for"

	^ self
		  severity: #info
		  kind: aKind
		  text: aString
		  request: aRequest
]

{ #category : 'instance creation' }
MetacelloResolutionFinding class >> severity: aSeverity kind: aKind text: aString request: aRequest [
	"A finding about aRequest, added to its findings"

	^ aRequest addFinding: (self new
			   setSeverity: aSeverity
			   kind: aKind
			   text: aString
			   request: aRequest)
]

{ #category : 'instance creation' }
MetacelloResolutionFinding class >> warning: aKind text: aString request: aRequest [

	^ self
		  severity: #warning
		  kind: aKind
		  text: aString
		  request: aRequest
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> isError [

	^ severity = #error
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> isInfo [

	^ severity = #info
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> isWarning [

	^ severity = #warning
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> kind [
	"What was found, as a symbol"

	^ kind
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> printOn: aStream [

	aStream
		nextPutAll: severity;
		nextPutAll: ': ';
		nextPutAll: text
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> request [
	"The request I am about: a MetacelloProjectRequest, or the MetacelloUserRequest for the load as a whole"

	^ request
]

{ #category : 'initialization' }
MetacelloResolutionFinding >> setSeverity: aSymbol kind: aKind text: aString request: aRequest [

	severity := aSymbol.
	kind := aKind.
	text := aString.
	request := aRequest
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> severity [

	^ severity
]

{ #category : 'accessing' }
MetacelloResolutionFinding >> text [

	^ text
]
