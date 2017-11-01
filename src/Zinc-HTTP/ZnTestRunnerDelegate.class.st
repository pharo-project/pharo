"
I am ZnTestRunnerDelegate, a web service interface to run unit tests.

	ZnTestRunnerDelegate startInServerOn: 1701.
	
	ZnEasy get: 'http://localhost:1701/sunit/ZnUtilsTests'.
	ZnEasy get: 'http://localhost:1701/sunit/ZnUtilsTests/testBase64'.

Web Service API:

	GET /sunit/MyTestCase
	GET /sunit/MyTestCase/testOne

Part of Zinc HTTP Components
"
Class {
	#name : #ZnTestRunnerDelegate,
	#superclass : #Object,
	#category : 'Zinc-HTTP-Client-Server'
}

{ #category : #public }
ZnTestRunnerDelegate class >> startInServerOn: port [
	"Start a new server bound to port on the local network running a test runner web service"

	"self startInServerOn: 1701"
	
	 ^ (ZnServer on: port)
			bindingAddress: NetNameResolver loopBackAddress;
			delegate: self new;
			start;
			yourself
]

{ #category : #'request handling' }
ZnTestRunnerDelegate >> handleGetRequest: request [
	| testCaseClass selector result |
	testCaseClass := Smalltalk globals
		at: request uri pathSegments second asSymbol
		ifAbsent: [ ^ self notFoundResponse: 'No such class, ', request uri pathSegments second ].
	(testCaseClass inheritsFrom: TestCase)
		ifFalse: [ ^ self notFoundResponse: testCaseClass printString, ' is not a TestCase subclass' ]. 
	selector := request uri pathSegments size = 3
		ifTrue: [ request uri pathSegments third asSymbol ]
		ifFalse: [ nil ].
	(selector isNil or: [ testCaseClass canUnderstand: selector ])
		ifFalse: [ ^ self notFoundResponse: selector, ' is not a method of ', testCaseClass printString ].
	result := selector 
		ifNil: [ testCaseClass suite run ] 
		ifNotNil: [ testCaseClass run: selector ].
	^ result runCount = result passedCount
		ifTrue: [ ZnResponse ok: (ZnEntity textCRLF: result printString) ]
		ifFalse: [ ZnResponse serverError: result printString ]
]

{ #category : #public }
ZnTestRunnerDelegate >> handleRequest: request [
	(request method = #GET 
		and: [ 
			request uri firstPathSegment = #sunit 
				and: [ 
					request uri pathSegments size > 1 ] ])
		ifTrue: [ 
			^ self handleGetRequest: request ].
	^ ZnResponse notFound: request uri
]

{ #category : #'request handling' }
ZnTestRunnerDelegate >> notFoundResponse: message [
	^ ZnResponse new
		statusLine: ZnStatusLine notFound;
		headers: ZnHeaders defaultResponseHeaders;
		entity: (ZnEntity textCRLF: 'Not Found: ', message);
		yourself.
]

{ #category : #public }
ZnTestRunnerDelegate >> value: request [
	"I implement the generic #value: message as equivalent to #handleRequest:"
	
	^ self handleRequest: request
]
