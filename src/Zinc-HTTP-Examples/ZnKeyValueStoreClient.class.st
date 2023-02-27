"
I am ZnKeyValueStoreClient, I access the REST API of ZnKeyValueStoreDelegate.

API

  #at: key - return the value stored under key, KeyNotFound if absent
  #at: key ifAbsent: block - return the value stored under key, evaluate block if absent
  #at: key put: value - store value under key
  #removeKey: key - remove key
  #contents - return a dictionary of all keys and values

Keys and values should be strings or will be converted using #asString.

See ZnKeyValueStoreTest for an executable example.

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnKeyValueStoreClient,
	#superclass : #Object,
	#instVars : [
		'httpClient'
	],
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #accessing }
ZnKeyValueStoreClient >> at: key [
	"Return the value of key as stored on the server, raising a KeyNotFound when necessary"

	^ [ self httpClient get: 'kvstore/', key asString ]
			on: ZnHttpUnsuccessful
			do: [ :exception |
				exception response isNotFound
					ifTrue: [ KeyNotFound signalFor: key ]
					ifFalse: [ exception pass ] ]
]

{ #category : #accessing }
ZnKeyValueStoreClient >> at: key ifAbsent: block [
	"Return the value of key as stored on the server, evaluating block when not found"

	^ [ self httpClient get: 'kvstore/', key asString ]
			on: ZnHttpUnsuccessful
			do: [ :exception |
				exception response isNotFound
					ifTrue: block
					ifFalse: [ exception pass ] ]
]

{ #category : #accessing }
ZnKeyValueStoreClient >> at: key put: value [
	"Store value under key on the server, returning the URI"

	^ self httpClient
		put: 'kvstore/', key asString
		contents: value asString
]

{ #category : #'initialize-release' }
ZnKeyValueStoreClient >> close [
	httpClient ifNotNil: [
		httpClient close.
		httpClient := nil ]
]

{ #category : #accessing }
ZnKeyValueStoreClient >> contents [
	"Return a Dictionary containing the full contents of the key-value store of the server"

	| dictionary dump position |
	dictionary := Dictionary new.
	dump := self httpClient get: 'kvstore'.
	(dump includes: $=) ifFalse: [ ^ dictionary ].
	dump lines do: [ :each |
		position := each indexOf: $=.
		dictionary
			at: (each copyFrom: 1 to: position -1)
			put: (each copyFrom: position + 1 to: each size) ].
	^ dictionary
]

{ #category : #accessing }
ZnKeyValueStoreClient >> httpClient [
	"Return the HTTP client that I (re)use.
	This ZnClient instance can be customized."

	^ httpClient ifNil: [ httpClient := self makeHttpClient ]
]

{ #category : #private }
ZnKeyValueStoreClient >> makeHttpClient [
	^ ZnClient new
		url: 'http://localhost:1701/kvstore';
		accept: ZnMimeType textPlain;
		enforceAcceptContentType: true;
		enforceHttpSuccess: true;
		autoResetEntityMethods: #(#HEAD #DELETE #GET);
		contentWriter: [ :data | ZnEntity text: data ];
		yourself
]

{ #category : #accessing }
ZnKeyValueStoreClient >> removeKey: key [
	"Remove key from the server, if present, returning the URI"

	^ self httpClient delete: 'kvstore/', key asString
]
