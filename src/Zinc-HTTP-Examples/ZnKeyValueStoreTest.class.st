"
Functional unit tests for the key-value store REST interface example
"
Class {
	#name : #ZnKeyValueStoreTest,
	#superclass : #TestCase,
	#instVars : [
		'client',
		'server'
	],
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #running }
ZnKeyValueStoreTest >> setUp [
	| port |
	super setUp.
	port := 10000 + 1000 atRandom.
	server := ZnServer startOn: port.
	server delegate map: 'kvstore' to: ZnKeyValueStoreDelegate new.
	client := ZnKeyValueStoreClient new.
	client httpClient port: port
]

{ #category : #running }
ZnKeyValueStoreTest >> tearDown [
	server stop.
	client close.
	super tearDown
]

{ #category : #tests }
ZnKeyValueStoreTest >> testUsage [
	| key value |
	key := 'key-' , 100 atRandom asString.
	value := 'value-' , 100 atRandom asString.

	self assert: client contents isEmpty.
	self should: [ client at: key ] raise: KeyNotFound.

	client at: key put: value.
	self assert: (client at: key) equals: value.

	client removeKey: key.
	self should: [ client at: key ] raise: KeyNotFound
]
