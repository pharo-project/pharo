"
I implement the traditional PKWARE encryption of ZIP members (""ZipCrypto""), as described in section 6.1 of the PKWARE APPNOTE.

WARNING: this cipher is weak. It is broken by known-plaintext attacks (Biham-Kocher, 1994) and offers no real confidentiality. I exist so that archives produced by other tools with ""zip -P"" or similar can be read, and so that such archives can be produced when interoperability requires it. Do not rely on me to protect sensitive data. AES-encrypted members (compression method 99, WinZip AE-x) and PKWARE strong encryption (bit 6 of the general purpose flag) are not supported; extracting them raises a ZipArchiveError.

An encrypted member's data is preceded by a 12-byte encryption header. Its last byte is a check byte, equal to the high byte of the member's CRC-32 or, when the member has a data descriptor, the high byte of its DOS modification time. It is used to detect a wrong password.

Usage:
	(ZipCrypto password: 'secret') encrypt: aByteArray.
	(ZipCrypto password: 'secret') decrypt: aByteArray.

A fresh instance must be used for each member, since the keys evolve with every byte processed.
"
Class {
	#name : 'ZipCrypto',
	#superclass : 'Object',
	#instVars : [
		'key0',
		'key1',
		'key2'
	],
	#category : 'Compression-Archives',
	#package : 'Compression',
	#tag : 'Archives'
}

{ #category : 'instance creation' }
ZipCrypto class >> headerSize [
	"Size in bytes of the encryption header preceding an encrypted member's data"

	^ 12
]

{ #category : 'instance creation' }
ZipCrypto class >> password: aStringOrByteArray [
	"Answer an instance whose keys are initialized from the given password.
	A String is encoded in UTF-8."

	^ self new initializeWithPassword: aStringOrByteArray
]

{ #category : 'private' }
ZipCrypto >> crc: aCrc byte: aByte [
	"One step of the raw CRC-32 update, without pre- or post-conditioning"

	^ (CRC crc32Table at: ((aCrc bitXor: aByte) bitAnd: 255) + 1) bitXor: (aCrc bitShift: -8)
]

{ #category : 'encrypting' }
ZipCrypto >> decrypt: aByteArray [

	^ aByteArray asByteArray collect: [ :each | self decryptByte: each ]
]

{ #category : 'encrypting' }
ZipCrypto >> decryptByte: aByte [

	| plain |
	plain := aByte bitXor: self streamByte.
	self updateKeys: plain.
	^ plain
]

{ #category : 'encrypting' }
ZipCrypto >> encrypt: aByteArray [

	^ aByteArray asByteArray collect: [ :each | self encryptByte: each ]
]

{ #category : 'encrypting' }
ZipCrypto >> encryptByte: aByte [

	| cipher |
	cipher := aByte bitXor: self streamByte.
	self updateKeys: aByte.
	^ cipher
]

{ #category : 'initialization' }
ZipCrypto >> initializeWithPassword: aStringOrByteArray [

	| bytes |
	bytes := aStringOrByteArray isString
		         ifTrue: [ aStringOrByteArray utf8Encoded ]
		         ifFalse: [ aStringOrByteArray asByteArray ].
	key0 := 305419896.
	key1 := 591751049.
	key2 := 878082192.
	bytes do: [ :each | self updateKeys: each ]
]

{ #category : 'private' }
ZipCrypto >> streamByte [

	| temp |
	temp := (key2 bitAnd: 16rFFFF) bitOr: 2.
	^ (temp * (temp bitXor: 1) bitShift: -8) bitAnd: 255
]

{ #category : 'private' }
ZipCrypto >> updateKeys: aByte [

	key0 := self crc: key0 byte: aByte.
	key1 := key1 + (key0 bitAnd: 255) bitAnd: 16rFFFFFFFF.
	key1 := key1 * 134775813 + 1 bitAnd: 16rFFFFFFFF.
	key2 := self crc: key2 byte: (key1 bitShift: -24)
]
