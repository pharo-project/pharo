"
An RFC4122 Universally Unique Identifier (UUID) is an opaque 128-bit number that can be used for identification purposes. Concretely, a UUID is a 16 element byte array.

The intent of UUIDs is to enable distributed systems to uniquely identify information without significant central coordination. In this context the word unique should be taken to mean ""practically unique"" rather than ""guaranteed unique"".

Usage:

```
	UUID new asString.
```	

Whenever you create a new UUID it will use your default UUID system library to generate one. If for some reason it is unable to work, the shared, default UUIDGenerator will be used to generate a new, unique UUID.

UUIDs have a standard string representation, like this:

	3ccb64f1-aa04-0d00-bbbc-259a0f871399
	
The representation consists of 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens).

My #printOn: #printString and #asString methods produce this representation.  My class' #fromString: or instance #readFrom: parse it.

Alternatively, my base 36 number representation is  the shortest representation still being able to work as filenames etc since it does not depend on case nor characters that might cause problems, and it is reasonably short.

See #asString36 and my class' #fromString36:

For time-based UUID (version 1) it is possible to recover a timestamp corresponding to the creation of the UUID.

See also:

  http://en.wikipedia.org/wiki/UUID
  https://tools.ietf.org/html/rfc4122
"
Class {
	#name : 'UUID',
	#superclass : 'Object',
	#instVars : [
		'uuidData'
	],
	#category : 'Network-UUID-Base',
	#package : 'Network-UUID',
	#tag : 'Base'
}

{ #category : 'instance creation' }
UUID class >> fromString36: aString [
	"Decode a UUID from a base 36 string using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems."

	| object array num |
	object := self nilUUID.
	array := object uuidData.
	num := Integer readFrom: aString asUppercase readStream base: 36.
	1 to: 16 do: [:i | array at: i put: (num byteAt: i)].
	object uuidData: array.
	^object
]

{ #category : 'instance creation' }
UUID class >> fromString: aString [
	"Read a UUID from aString with my official representation, 32 lowercase hexadecimal (base 16) digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens)"

	| uuid |
	aString size ~= 36 ifTrue: [ self error: 'a UUID should be 36 characters' ].
	uuid := self nilUUID.
	uuid readFrom: aString readStream.
	^ uuid
]

{ #category : 'instance creation' }
UUID class >> new [
	|uuid|
	uuid := super new.
	uuid uuidData generateUUIDInPlace.
	^uuid
]

{ #category : 'instance creation' }
UUID class >> nilUUID [
	"Return a empty un-initialized UUID, with all zero values, 00000000-0000-0000-0000-000000000000"

	^ super new
]

{ #category : 'instance creation' }
UUID class >> nonStandardUUID [
	"This method uses the legacy UUID generation method from Pharo."
	
	^ self nonStandardUUIDWithGenerator: UUIDGenerator default
]

{ #category : 'instance creation' }
UUID class >> nonStandardUUIDWithGenerator: aGenerator [
	"This method uses the legacy UUID generation method from Pharo."
	
	|uuid|
	uuid := super new.
	aGenerator placeFields: uuid uuidData.
	^uuid
]

{ #category : 'comparing' }
UUID >> < aMagnitude [
	"Answer whether the receiver is less than the argument."

	uuidData size = aMagnitude uuidData size ifFalse: [
		^ uuidData size < aMagnitude uuidData size ].
	1 to: uuidData size do: [ :i |
		(uuidData at: i) = (aMagnitude uuidData at: i) ifFalse: [
			^ (uuidData at: i) < (aMagnitude uuidData at: i) ] ].
	^ false
]

{ #category : 'comparing' }
UUID >> <= aMagnitude [
	"Answer whether the receiver is less than or equal to the argument."

	^(self > aMagnitude) not
]

{ #category : 'comparing' }
UUID >> = anUUID [
	^self species == anUUID species and: [ 
		(self uuidData = anUUID uuidData) ]
	
]

{ #category : 'comparing' }
UUID >> > aMagnitude [
	"Answer whether the receiver is greater than the argument."

	^aMagnitude < self
]

{ #category : 'comparing' }
UUID >> >= aMagnitude [
	"Answer whether the receiver is greater than or equal to the argument."

	^(self < aMagnitude) not
]

{ #category : 'accessing' }
UUID >> asInteger [
	
	"Should be deprecated, but is still used by iceberg."
	"self
		deprecated:
		'The UUIDs are no longer a subclass of ByteArray, and should not be directly modified.'."
	^ uuidData asInteger
]

{ #category : 'converting' }
UUID >> asString36 [
	"Return a String encoding the receiver as a base 36 number using 0-9 and lowercase a-z.
	This is the shortest representation still being able to work as
	filenames etc since it does not depend on case nor characters
	that might cause problems, and it is reasonably short."

	| num |
	num := 0.
	1 to: uuidData size do: [ :i | num := num + ((256 raisedTo: i - 1) * (uuidData at: i)) ].
	^ (num printStringBase: 36) asLowercase
]

{ #category : 'accessing' }
UUID >> at: aNumber [
	
	"Should be deprecated, but is still used by iceberg."
	"self
		deprecated:
		'The UUIDs are no longer a subclass of ByteArray, and should not be directly modified.'."
	^ uuidData at: aNumber
]

{ #category : 'accessing' }
UUID >> at: aNumber put: aByte [
	
	"Should be deprecated, but is still used by iceberg."
	"self
		deprecated:
		'The UUIDs are no longer a subclass of ByteArray, and should not be directly modified.'."
	uuidData at: aNumber put: aByte
]

{ #category : 'comparing' }
UUID >> clockSequence [
	"Returns the clock sequence for Verison 1 UUIDs. See https://tools.ietf.org/html/rfc4122"

	self isTimeBased ifFalse: [
		self error: 'Only time-based UUIDs have a timestamp.' ].
	^ (uuidData at: 9) & 16r3f << 8 + (uuidData at: 10)
]

{ #category : 'comparing' }
UUID >> hash [
	^uuidData hash
]

{ #category : 'initialization' }
UUID >> initialize [
	uuidData := ByteArray new: 16.
]

{ #category : 'comparing' }
UUID >> isDCEBased [
	"Checks if UUID is version 2. See https://tools.ietf.org/html/rfc4122"
	
	^ self isRFC4122 and: (self version = 2)
]

{ #category : 'comparing' }
UUID >> isMD5NameBased [
	"Checks if UUID is version 3. See https://tools.ietf.org/html/rfc4122"

	^ self isRFC4122 and: (self version = 3)
]

{ #category : 'testing' }
UUID >> isNilUUID [
	"Return true when I am equal to the nilUUID, 00000000-0000-0000-0000-000000000000, false otherwise"

	^ uuidData allSatisfy: [ :each | each isZero ]
]

{ #category : 'comparing' }
UUID >> isRFC4122 [
	"Checks if UUID is built according to RFC 4122. See https://tools.ietf.org/html/rfc4122"

	^ self variant & 2r110 = 2r100
]

{ #category : 'comparing' }
UUID >> isRandomBased [
	"Checks if UUID is version 4. See https://tools.ietf.org/html/rfc4122"

	^ self isRFC4122 and: (self version = 4)
]

{ #category : 'comparing' }
UUID >> isSHA1NameBased [
	"Checks if UUID is version 5. See https://tools.ietf.org/html/rfc4122"

	^ self isRFC4122 and: (self version = 5)
]

{ #category : 'comparing' }
UUID >> isTimeBased [
	"Checks if UUID is version 1. See https://tools.ietf.org/html/rfc4122"

	^ self isRFC4122 and: (self version = 1)
]

{ #category : 'comparing' }
UUID >> node [
	"Returns the node of the UUID as a ByteArray. See https://tools.ietf.org/html/rfc4122"

	^uuidData copyFrom: 11 to: 16.
]

{ #category : 'printing' }
UUID >> printOn: aStream [
	"Print my official representation, 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens) on aStream"

	1 to: 4 do: [ :i | (uuidData at: i) printLowercaseHexByteOn: aStream ].
	aStream nextPut: $-.
	5 to: 6 do: [ :i | (uuidData at: i) printLowercaseHexByteOn: aStream ].
	aStream nextPut: $-.
	7 to: 8 do: [ :i | (uuidData at: i) printLowercaseHexByteOn: aStream ].
	aStream nextPut: $-.
	9 to: 10 do: [ :i | (uuidData at: i) printLowercaseHexByteOn: aStream ].
	aStream nextPut: $-.
	11 to: 16 do: [ :i | (uuidData at: i) printLowercaseHexByteOn: aStream ]
]

{ #category : 'printing' }
UUID >> printString [
	"Return a String with my official representation, 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens)"

	^ String
		new: 36
		streamContents: [ :stringStream | self printOn: stringStream ]
]

{ #category : 'initialization' }
UUID >> readFrom: aStream [
	"Read my official representation, 32 lowercase hexadecimal digits, displayed in five groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36 characters (32 alphanumeric characters and four hyphens) from aStream"

	1 to: 4 do: [ :i | uuidData at: i put: (Integer readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	5 to: 6 do: [ :i | uuidData at: i put: (Integer readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	7 to: 8 do: [ :i | uuidData at: i put: (Integer readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	9 to: 10 do: [ :i | uuidData at: i put: (Integer readHexByteFrom: aStream) ].
	aStream next = $- ifFalse: [ self error: '- separator expected' ].
	11 to: 16 do: [ :i | uuidData at: i put: (Integer readHexByteFrom: aStream) ]
]

{ #category : 'accessing' }
UUID >> timestamp [
	"Returns the timestamp of the version 1 UUID as a local DateAndTime. See https://tools.ietf.org/html/rfc4122"

	| timestamp |
	self isTimeBased ifFalse: [
		self error: 'Only time-based UUIDs have a timestamp.' ].

	timestamp := (uuidData at: 7) & 16r0f.
	"The timestamp bytes in the UUID bytearray are scrambled. Iterate them in the good order"
	#( 8 5 6 1 2 3 4 ) do: [ :i |
		timestamp := timestamp << 8 + (uuidData at: i) ].

	^ (DateAndTime
		   year: 1582
		   month: 10
		   day: 15
		   offset: 0 hours) + (timestamp * 100) nanoSeconds offset:
		  DateAndTime localOffset
]

{ #category : 'accessing' }
UUID >> uuidData [

	^ uuidData
]

{ #category : 'accessing' }
UUID >> uuidData: anObject [

	uuidData := anObject
]

{ #category : 'comparing' }
UUID >> variant [
	"Returns the variant of the UUID. See https://tools.ietf.org/html/rfc4122"

	^ (uuidData at: 9) >> 5
]

{ #category : 'comparing' }
UUID >> version [
	"Returns the version of the UUID. See https://tools.ietf.org/html/rfc4122"

	^ (uuidData at: 7) >> 4
]
