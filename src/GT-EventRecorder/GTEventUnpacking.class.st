"
I represent an unpacking object that is able to receive serialized objects, e.g., FUEL or STON, and tries all available packing systems to unpack the serialized objects.

Others can ask me for materializing an object by sending me #unpack:. I resent the request to GTEventPacking objects and if anyone is able to materialize it, I will return the materialized object. Otherwise, I raise an error.

Collaborators Part: GTEventPacking, GTEventInitializationError, GTEventUnpackingError.

Public API and Key Messages

- unpack: aByteArray
- addPacking: aGTEventPacking
- how to create instances: #default on the class side.

Example:
	(GTEventUnpacking default
		unpack: (GTEventPacking ston 
					pack: (GTEventCollector new 
							add: 1; add: 2; add: 3; yourself)) 
			data) unpackedData
 
Internal Representation and Key Implementation Points.

    Instance Variables
	packings:		<OrderedCollection>

"
Class {
	#name : #GTEventUnpacking,
	#superclass : #Object,
	#instVars : [
		'packings'
	],
	#category : #'GT-EventRecorder-Support'
}

{ #category : #'instance creation' }
GTEventUnpacking class >> default [
	^ self new
			addPacking: GTEventPacking fuel;
			addPacking: GTEventPacking ston;
			yourself
]

{ #category : #adding }
GTEventUnpacking >> addPacking: aGTEventPacking [ 
	^ self packings addLast: aGTEventPacking
]

{ #category : #accessing }
GTEventUnpacking >> packings [
	^ packings ifNil: [ packings := OrderedCollection new ]
]

{ #category : #packing }
GTEventUnpacking >> safeUnpackDirectory: aFileReference [
	"self default safeUnpackDirectory: './gt/events/' asFileReference"
	| unpackedData exceptions |
	exceptions := OrderedCollection new.
	[ unpackedData := self unpackDirectory: aFileReference ] on: GTEventUnpackingError do: [ :exception |
		exceptions addLast: exception.
		exception resume ].
	^ unpackedData -> exceptions
]

{ #category : #packing }
GTEventUnpacking >> unpack: aByteArray [
	| exceptions |
	exceptions := OrderedCollection new: self packings size.
	self packings do: [ :eachEventPacking |
		[ ^ eachEventPacking unpack: aByteArray ] on: Error
			do: [ :exception | exceptions addLast: exception ] ].
	exceptions
		ifEmpty: [ GTEventInitializationError signal: 'Uninitialized unpacking system' ]
		ifNotEmpty: [ GTEventUnpackingError 
								signal: 'Cannot unpack data' 
								data: aByteArray 
								errors: exceptions ].
	"In case of resumed exception it returns nil object."
	^ nil
]

{ #category : #packing }
GTEventUnpacking >> unpackDirectory: aFileReference [
	"self default unpackDirectory: './gt/events/' asFileReference"
	^ aFileReference asFileReference allFiles 
			collect: [ :eachFile | 
				(eachFile basename = '.DS_Store') 
					ifTrue: [ nil ]
					ifFalse: [ self unpackFile: eachFile ] ]
			thenSelect: #notNil
]

{ #category : #packing }
GTEventUnpacking >> unpackFile: aFileReference [
	"self default unpackFile: './gt/events/' asFileReference allFiles atRandom"
	^ [ self unpack: (aFileReference binaryReadStreamDo: [ :aStream |  aStream contents ]) ]
		on: GTEventUnpackingError do: [ :anException |
			anException data: aFileReference.
			anException pass ]
]
