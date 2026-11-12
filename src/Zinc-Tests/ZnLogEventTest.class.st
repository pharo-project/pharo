Class {
	#name : 'ZnLogEventTest',
	#superclass : 'TestCase',
	#category : 'Zinc-Tests',
	#package : 'Zinc-Tests'
}

{ #category : 'accessing' }
ZnLogEventTest >> clientId [
	^ 'client-1'
]

{ #category : 'accessing' }
ZnLogEventTest >> request [
	^ ZnRequest get: 'http://foo.com/test'
]

{ #category : 'accessing' }
ZnLogEventTest >> response [
	^ ZnResponse ok: (ZnEntity text: 'OK')
]

{ #category : 'accessing' }
ZnLogEventTest >> serverId [
	^ 'server-1'
]

{ #category : 'testing' }
ZnLogEventTest >> testClientTransactionEvent [
	| event |
	(event := ZnClientTransactionEvent new)
		clientId: self clientId;
		request: self request;
		response: self response;
		requestDuration: self timing requestDuration;
		responseDuration: self timing responseDuration.
	self assert: event id isInteger.
	self assert: event processId isInteger.
	self assert: event timestamp <= DateAndTime now.
	self assert: event clientId equals: self clientId.
	self assert: event request uri equals: self request uri.
	self assert: event request method equals: self request method.
	self assert: event response code equals: self response code.
	self assert: event duration equals: self timing requestDuration + self timing responseDuration.
	self assert: event printString isString.
	^ event
]

{ #category : 'testing' }
ZnLogEventTest >> testServerTransactionEvent [
	| event |
	(event := ZnServerTransactionEvent new)
		serverId: self serverId;
		request: self request;
		response: self response;
		timing: self timing.
	self assert: event id isInteger.
	self assert: event processId isInteger.
	self assert: event timestamp <= DateAndTime now.
	self assert: event serverId equals: self serverId.
	self assert: event request uri equals: self request uri.
	self assert: event request method equals: self request method.
	self assert: event response code equals: self response code.
	self assert: event duration equals: self timing totalDuration.
	self assert: event printString isString.
	^ event
]

{ #category : 'testing' }
ZnLogEventTest >> testSimplifiedClientTransactionEvent [
	| event |
	(event := ZnSimplifiedClientTransactionEvent new)
		clientId: self clientId;
		request: self request;
		response: self response;
		requestDuration: self timing requestDuration;
		responseDuration: self timing responseDuration.
	self assert: event id isInteger.
	self assert: event processId isInteger.
	self assert: event timestamp <= DateAndTime now.
	self assert: event clientId equals: self clientId.
	self assert: event url equals: self request uri.
	self assert: event method equals: self request method.
	self assert: event responseCode equals: self response code.
	self assert: event size equals: self response contentLength.
	self assert: event duration equals: self timing requestDuration + self timing responseDuration.
	self assert: event printString isString.
	^ event
]

{ #category : 'testing' }
ZnLogEventTest >> testSimplifiedServerTransactionEvent [
	| event |
	(event := ZnSimplifiedServerTransactionEvent new)
		serverId: self serverId;
		request: self request;
		response: self response;
		timing: self timing.
	self assert: event id isInteger.
	self assert: event processId isInteger.
	self assert: event timestamp <= DateAndTime now.
	self assert: event serverId equals: self serverId.
	self assert: event url equals: self request uri.
	self assert: event method equals: self request method.
	self assert: event responseCode equals: self response code.
	self assert: event size equals: self response contentLength.
	self assert: event duration equals: self timing totalDuration.
	self assert: event printString isString.
	^ event
]

{ #category : 'accessing' }
ZnLogEventTest >> timing [
	^ ZnServerTransactionTiming new
			requestDuration: 1;
			responseDuration: 2;
			yourself
]
