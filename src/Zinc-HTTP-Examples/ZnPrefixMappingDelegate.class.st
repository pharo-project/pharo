"
I'm a ZnPrefixMappingDelegate. I provide mapping of handlers separated by URL prefix. A URI prefix is the first path segment in an URI. 

	apiHandler := MyRESTApiHandler new.
	testRunnerHandler := ZnTestRunnerDelegate new.

	prefixMappingDelegate := ZnPrefixMappingDelegate new
		map: 'api' to: apiHandler;
		map: 'tests' to: testRunnerHandler.

URI paths are consumed when the mapping happens so ZnPrefixMappingDelegate can be nested. 

	prefixMappingDelegate := ZnPrefixMappingDelegate new
		map: 'api' to: (ZnPrefixMappingDelegate new
			map: 'v2' to: apiHandler.
		map: 'tests' to: testRunnerHandler.

"
Class {
	#name : #ZnPrefixMappingDelegate,
	#superclass : #Object,
	#instVars : [
		'prefixDictionary'
	],
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #public }
ZnPrefixMappingDelegate >> handleRequest: request [
	| handler relativeRequest |
	handler := self prefixDictionary
		at: request uri firstPathSegment
		ifAbsent: [ ^ ZnResponse notFound: request uri ].
	relativeRequest := request copy.
	relativeRequest uri removeFirstPathSegment.
	^ handler isBlock
		ifTrue: [ handler value: relativeRequest ]
		ifFalse: [ handler handleRequest: relativeRequest ]
]

{ #category : #public }
ZnPrefixMappingDelegate >> map: uriPrefixString to: handler [
	self prefixDictionary
		at: uriPrefixString
		put: handler
]

{ #category : #accessing }
ZnPrefixMappingDelegate >> prefixDictionary [
	^ prefixDictionary ifNil: [
		prefixDictionary := Dictionary new ]
]
