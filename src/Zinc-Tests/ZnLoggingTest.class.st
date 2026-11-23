Class {
	#name : 'ZnLoggingTest',
	#superclass : 'TestCase',
	#instVars : [
		'logEventCollection'
	],
	#category : 'Zinc-Tests',
	#package : 'Zinc-Tests'
}

{ #category : 'running' }
ZnLoggingTest >> setUp [

	super setUp.

	logEventCollection := OrderedCollection new.
	
	ZnLogEvent announcer
		when: ZnLogEvent
		do: [ :event | logEventCollection add: event ]
		for: self
]

{ #category : 'accessing' }
ZnLoggingTest >> smallHtmlUrl [
	^ 'http://zn.stfx.eu/zn/small.html' asZnUrl
]

{ #category : 'running' }
ZnLoggingTest >> tearDown [

	"thisContext crTrace."
	
	ZnLogEvent announcer unsubscribe: self.
	logEventCollection removeAll.
	logEventCollection := nil.
	
	super tearDown
]

{ #category : 'tests' }
ZnLoggingTest >> testClientLogging [

	| client count |
	(client := ZnClient new)
		clientId: #C1;
		beOneShot.
	client get: self smallHtmlUrl.
	client close.
	count := ZnLogEvent announcer numberOfSubscriptions.
	ZnLogEvent announcer unsubscribe: self.
	self
		assert: ZnLogEvent announcer numberOfSubscriptions
		equals: count - 1.
	self deny: logEventCollection isEmpty.
	self assert: logEventCollection anyOne clientId equals: #C1
]

{ #category : 'tests' }
ZnLoggingTest >> testGet [
	ZnServer withOSAssignedPortDo: [ :server |
		ZnClient new
			systemPolicy;
			url: server localUrl / #small;
			get ].
	self assert: logEventCollection notEmpty
]

{ #category : 'tests' }
ZnLoggingTest >> testServerLogging [

	| client count renderedLogEvents |
	ZnServer withOSAssignedPortDo: [ :server |
			1 to: 3 do: [ :logLevel |
					server
						serverId: ('S-{1}' format: { logLevel });
						logLevel: logLevel.
					client := ZnClient new.
					client
						clientId: ('C-{1}' format: { logLevel });
						logLevel: logLevel.
					client get: (server localUrl addPathSegment: #small).
					self assert: client isSuccess.
					client get: (server localUrl addPathSegment: #error).
					self deny: client isSuccess.
					server delegate
						map: 'redirect'
						to: [ :request | ZnResponse redirect: #welcome ].
					client get: (server localUrl addPathSegment: #redirect).
					self assert: client isSuccess.
					client close ] ].
	count := ZnLogEvent announcer numberOfSubscriptions.
	ZnLogEvent announcer unsubscribe: self.
	self
		assert: ZnLogEvent announcer numberOfSubscriptions
		equals: count - 1.
	self deny: logEventCollection isEmpty.
	renderedLogEvents := String streamContents: [ :out |
			                     logEventCollection do: [ :event |
					                     out
						                     print: event;
						                     cr ] ].
	self deny: renderedLogEvents isEmpty
]
