"
I represent a String type in uffi that gets marshalled to a pointer type in libffi.
I override the marshalling methods to allow reading/writing strings from/to C pointers.

See my superclass for more details.
"
Class {
	#name : 'TFStringType',
	#superclass : 'TFDerivedType',
	#classVars : [
		'AllocatedStrings'
	],
	#category : 'ThreadedFFI-Types',
	#package : 'ThreadedFFI',
	#tag : 'Types'
}

{ #category : 'writing' }
TFStringType >> allocateString: aString [
	| anExternalAddress |

	anExternalAddress := (ExternalAddress fromString: aString).
	self allocatedStrings add: anExternalAddress.
	^ anExternalAddress
]

{ #category : 'accessing' }
TFStringType >> allocatedStrings [

	^ AllocatedStrings ifNil: [ AllocatedStrings := Set new ]
]

{ #category : 'writing' }
TFStringType >> callbackReadValue: anExternalAddress [

	^ (self readValue: anExternalAddress) utf8StringFromCString
]

{ #category : 'marshalling' }
TFStringType >> emitFreeIfNeededOfIndex: argIndex argument: argument withBuilder: anIRBuilder [

	anIRBuilder pushLiteral: self.
	anIRBuilder pushTemp: argument tempName.
	anIRBuilder send: #freeValueIfNeeded:.
	anIRBuilder popTop
]

{ #category : 'marshalling' }
TFStringType >> emitMarshallFromPrimitive: anIRBuilder [

	anIRBuilder send: #utf8StringFromCString
]

{ #category : 'marshalling' }
TFStringType >> emitMarshallToPrimitive: anIRBuilder argument: anArg [

	anIRBuilder addTemp: anArg tempName.
	anIRBuilder storeTemp: anArg tempName.
	anIRBuilder popTop.
	anIRBuilder pushLiteral: self.
	anIRBuilder pushTemp: anArg tempName.
	anIRBuilder send: #prepareStringForMarshalling:.
	anIRBuilder storeTemp: anArg tempName.

]

{ #category : 'freeing' }
TFStringType >> freeValueIfNeeded: aCHeapValueHolder [
	| pointer |

	aCHeapValueHolder isNull
		ifTrue: [ ^ self ].

	pointer := aCHeapValueHolder.

	"If the pointer is a pointer to the string, and not the string directly"
	(self allocatedStrings includes: pointer) ifFalse: [
		pointer := aCHeapValueHolder pointerAt: 1.
		(self allocatedStrings includes: pointer)
			ifFalse: [ ^ self ]].

	self allocatedStrings remove: pointer.
	pointer isNull ifFalse: [ pointer free ]
]

{ #category : 'marshalling' }
TFStringType >> marshallToPrimitive: aValue [

	^ self prepareStringForMarshalling: aValue
]

{ #category : 'writing' }
TFStringType >> prepareStringForMarshalling: aStringOrExternalAddress [

	"The TFString type supports four possible parameters.

	- A String: this is allocated as an external string, encoded and passed the pointer to the allocated space.
	- A ByteArray: this will be passed as a pointer to the external call. It should be pinned and dereferenced to get the address.
	- An ExternalAddress: this is directly passed to the external call.
	- nil: An ExternalAddress null is passed in this case "

	"Maybe this code should be implemented delegating to the objects to handle the different cases"

	"Handling Strings"
	aStringOrExternalAddress isString
		ifTrue: [ ^ self allocateString: aStringOrExternalAddress ].

	"Handling nil"
		aStringOrExternalAddress
			ifNil: [ ^ ExternalAddress null ].

	"Handling ExternalAddress"
		aStringOrExternalAddress isExternalAddress
			ifTrue: [ ^ aStringOrExternalAddress ].

	"Handling ByteArray - We have to first check for ExternalAddress, as ExternalAddress are ByteArray"
		(aStringOrExternalAddress isKindOf: ByteArray)
			ifTrue: [ aStringOrExternalAddress pinInMemory.
            ^ PointerUtils oopForObject: aStringOrExternalAddress ].

	"Handling ExternalAddress"
		^ self error: 'Could not handle this object'
]

{ #category : 'writing' }
TFStringType >> readValue: anExternalAddress [

	^ self basicType readValue: anExternalAddress
]

{ #category : 'writing' }
TFStringType >> write: aStringOrExternalAddress into: targetAddress [

	"If the argument is a aString I have to allocate it and later free it"
	| anAddress |
	anAddress := self prepareStringForMarshalling: aStringOrExternalAddress.

	self basicType
		write: anAddress
		into: targetAddress
]
