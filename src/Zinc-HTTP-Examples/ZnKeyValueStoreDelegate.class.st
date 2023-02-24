"
I am ZnKeyValueStoreDelegate, I implement an example REST interface to a simple key-value store.

API

 GET /kvstore - list the full contents of the key-value store
 GET /kvstore/some-key - return the value stored under some-key, or NotFound
 PUT /kvstore/some-key - store the entity text as value for some-key
 DELETE /kvstore/some-key - delete the value stored under some-key, or NotFound

in all other cases, return a Bad Request.

Keys should be strings. Value enities should be text/plain. 

You can use your browser to see the full key-value store at http://localhost:1701/kvstore

See ZnKeyValueStoreClient for a concrete implementation of accessing me.
See my class side's #installInDefaultServer for one way to deploy me.
See ZnKeyValueStoreTest>>#setUp for an example of deploying me using a different port.

Here is a command line session using curl, interacting with me:

$ curl http://localhost:1701/kvstore
the key-value store is empty

$ curl -X PUT -d 'ABC' -H'Content-Type:text/plain' http://localhost:1701/kvstore/xyz
ABC

$ curl http://localhost:1701/kvstore/xyz
ABC

$ curl http://localhost:1701/kvstore
xyz = ABC

$ curl -X DELETE http://localhost:1701/kvstore/xyz
/kvstore/xyz

$ curl http://localhost:1701/kvstore/xyz
Not Found /kvstore/xyz

Note that some newlines were added for readabilty. 

Part of Zinc HTTP Components.
"
Class {
	#name : #ZnKeyValueStoreDelegate,
	#superclass : #Object,
	#instVars : [
		'data',
		'access'
	],
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #public }
ZnKeyValueStoreDelegate class >> installInDefaultServer [
	"Assuming the default server has the default delegate,
	install ourself under /kvstore"

	ZnServer default delegate
		map: 'kvstore' to: self new
]

{ #category : #public }
ZnKeyValueStoreDelegate >> handleDeleteKey: request [
	"Handle DELETE /kvstore/sone-key returing either the URI or a 404 Not Found"

	access critical: [
		data
			removeKey: request uri pathSegments second
			ifAbsent: [ ^ ZnResponse notFound: request uri ].
		^ ZnResponse ok: (ZnEntity text: request uri asString) ]
]

{ #category : #public }
ZnKeyValueStoreDelegate >> handleGetKey: request [
	"Handle GET /kvstore/sone-key returing either the value or a 404 Not Found"

	^ access critical: [
			data
				at: request uri pathSegments second
				ifPresent: [ :value | ZnResponse ok: (ZnEntity text: value) ]
				ifAbsent: [ ZnResponse notFound: request uri ] ]
]

{ #category : #public }
ZnKeyValueStoreDelegate >> handlePutKey: request [
	"Handle PUT /kvstore/sone-key returing either the value.
	The value is the contents of the entity, assumed to be text/plain"

	access critical: [
		data
			at: request uri pathSegments second
			put: request entity contents ].
	^ ZnResponse ok: (ZnEntity text: request entity contents)
]

{ #category : #public }
ZnKeyValueStoreDelegate >> handleRequest: request [
	"Dispatch on the different REST calls that I implement"

	^ request uri pathSegments size = 1
		ifTrue: [ self handleStoreContents ]
		ifFalse: [
			request uri pathSegments size = 2
				ifTrue: [
					request method = #GET
						ifTrue: [ ^ self handleGetKey: request ].
					request method = #DELETE
						ifTrue: [ ^ self handleDeleteKey: request ].
					(request method = #PUT
							and: [ request entity contentType matches: ZnMimeType textPlain ])
						ifTrue: [ ^ self handlePutKey: request ] ].
			ZnResponse badRequest: request ]
]

{ #category : #public }
ZnKeyValueStoreDelegate >> handleStoreContents [
	"Handle GET /kvstore - list the full contents of the key-valyue store"

	^ ZnResponse ok: (ZnEntity text: self storeContents)
]

{ #category : #initialization }
ZnKeyValueStoreDelegate >> initialize [
	super initialize.
	data := Dictionary new.
	access := Monitor new
]

{ #category : #private }
ZnKeyValueStoreDelegate >> storeContents [
	"Generate a string describing/listing the full contents of my key-value store"

	^ String streamContents: [ :out |
			access critical: [
				data isEmpty
					ifTrue: [ out << 'the key-value store is empty'; crlf ]
					ifFalse: [
						data keysAndValuesDo: [ :key :value |
							out << key << ' = ' << value; crlf ] ] ] ]
]

{ #category : #public }
ZnKeyValueStoreDelegate >> value: request [
	"I implement the generic #value: message as equivalent to #handleRequest:"

	^ self handleRequest: request
]
