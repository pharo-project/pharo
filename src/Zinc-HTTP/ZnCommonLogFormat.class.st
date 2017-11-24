"
I am ZnCommonLogFormat, I can output ZnServerTransactionEvent objects using Apache Common Log Format (CLF).

https://en.wikipedia.org/wiki/Common_Log_Format
https://httpd.apache.org/docs/trunk/logs.html#common

| formatter |
formatter := ZnCommonLogFormat new.
ZnLogEvent announcer 
  when: ZnServerTransactionEvent 
  do: [ :event | 
    formatter format: event on: Transcript.
    Transcript cr; endEntry ].

"
Class {
	#name : #ZnCommonLogFormat,
	#superclass : #Object,
	#instVars : [
		'combined'
	],
	#category : #'Zinc-HTTP-Logging'
}

{ #category : #private }
ZnCommonLogFormat >> clientIPAddressFrom: event [
	^ event request headers 
		at: ZnConstants remoteAddressHeader 
		ifAbsent: [ '-' ]
]

{ #category : #accessing }
ZnCommonLogFormat >> combined [
	^ combined
]

{ #category : #accessing }
ZnCommonLogFormat >> combined: boolean [
	"If boolean is true (the default), use the 'Combined' format, 
	adding Referer and User-Agent fields."
	
	combined := boolean
]

{ #category : #public }
ZnCommonLogFormat >> format: event on: stream [
	"Output a ZnServerTransactionEvent on a text stream in Common Log Format"
	
	stream << (self clientIPAddressFrom: event); << ' - '.
	stream << (self userFrom: event); space.
	self formatTimestamp: event timestamp on: stream.
	self formatRequest: event on: stream.
	self formatResponse: event on: stream. 
	combined ifTrue: [ self formatCombinedFields: event request on: stream ]
]

{ #category : #private }
ZnCommonLogFormat >> formatCombinedFields: request on: stream [
	| referer userAgent |
	referer := request headers at: 'Referer' ifAbsent: [ '-' ].
	userAgent := request headers at: 'User-Agent' ifAbsent: [ '-' ].
	stream space.
	stream nextPut: $"; << referer; nextPut: $".
	stream space.
	stream nextPut: $"; << userAgent; nextPut: $"

]

{ #category : #private }
ZnCommonLogFormat >> formatRequest: event on: stream [
	| requestLine |
	stream space.
	requestLine := event request requestLine.
	stream << $".
	requestLine printMethodAndUriOn: stream.
	stream space; << requestLine version.
	stream << $"
]

{ #category : #private }
ZnCommonLogFormat >> formatResponse: event on: stream [
	| size |
	stream space.
	stream print: event response code.
	stream space.
	size := event response hasEntity 
		ifTrue: [ event response contentLength ] 
		ifFalse: [ 0 ].
	stream print: size

]

{ #category : #private }
ZnCommonLogFormat >> formatTimestamp: timestamp on: stream [
	stream nextPut: $[.
	timestamp dayOfMonth printOn: stream base: 10 length: 2 padded: true.
	stream nextPut: $/.
	stream << timestamp monthAbbreviation.
	stream nextPut: $/.
	timestamp year printOn: stream.
	stream << $:.
	timestamp printHMSOn: stream.
	stream space.
	stream nextPut: (timestamp offset positive ifTrue: [ $+ ] ifFalse: [ $- ]).
	timestamp offset hours abs printOn: stream base: 10 length: 2 padded: true.
	stream nextPut: $:.
	timestamp offset minutes abs printOn: stream base: 10 length: 2 padded: true.
	stream nextPut: $]
]

{ #category : #initialize }
ZnCommonLogFormat >> initialize [
	super initialize.
	combined := true.
]

{ #category : #private }
ZnCommonLogFormat >> userFrom: event [
	^ [ event request basicAuthentication first ]
		on: Error
		do: [ '-' ]
]
