"
I test the traditional PKWARE encryption of ZIP members (ZipCrypto, APPNOTE section 6.1): the cipher itself, reading archives encrypted by Info-ZIP ""zip -P"", writing encrypted members, the detection of missing and wrong passwords, and the clear error raised for AES and strong encryption, which are not supported.

The fixtures were produced with ""zip -X -P secret"" (Info-ZIP 3.0). Their members have a data descriptor, so their encryption check byte comes from the modification time, whereas members written by Pharo have none and use the CRC.
"
Class {
	#name : 'ZipCryptoTest',
	#superclass : 'TestCase',
	#category : 'Compression-Tests-Archives',
	#package : 'Compression-Tests',
	#tag : 'Archives'
}

{ #category : 'fixtures' }
ZipCryptoTest >> bigText [

	^ String new: 1200 streamContents: [ :s | 200 timesRepeat: [ s nextPutAll: 'pharo ' ] ]
]

{ #category : 'fixtures' }
ZipCryptoTest >> deflatedArchiveBytes [
	"zip -X -P secret -9 deflated.zip big.txt, where big.txt contains 'pharo ' 200 times"

	^ 'UEsDBBQACwAIAKd5XF2NomSUHgAAALAEAAAHAAAAYmlnLnR4dEbmu4UHpstivrl1U0tjn3CgIkvxWJjmQsgaG6p5QlBLBwiNomSUHgAAALAEAABQSwECHgMUAAsACACneVxdjaJklB4AAACwBAAABwAAAAAAAAABAAAApIEAAAAAYmlnLnR4dFBLBQYAAAAAAQABADUAAABTAAAAAAA='
		  base64Decoded
]

{ #category : 'helpers' }
ZipCryptoTest >> firstMemberOf: aByteArray [

	^ (ZipArchive new readFrom: aByteArray readStream) members first
]

{ #category : 'helpers' }
ZipCryptoTest >> patchMethod: aMethod flag: aFlag in: aByteArray [
	"Answer a copy of the single-member archive aByteArray whose compression method
	and general purpose flag are replaced, in the local and central headers"

	| bytes central |
	bytes := aByteArray copy.
	central := (bytes indexOfSubCollection: #[ 16r50 16r4B 16r01 16r02 ] startingAt: 1) - 1.
	{
		6.
		(central + 8) } do: [ :offset |
			bytes at: offset + 1 put: (aFlag bitAnd: 255).
			bytes at: offset + 2 put: (aFlag bitShift: -8).
			bytes at: offset + 3 put: (aMethod bitAnd: 255).
			bytes at: offset + 4 put: (aMethod bitShift: -8) ].
	^ bytes
]

{ #category : 'helpers' }
ZipCryptoTest >> should: aBlock raise: anErrorClass withMessageIncluding: aString [

	| raised |
	raised := false.
	aBlock
		on: anErrorClass
		do: [ :e |
				raised := true.
				self assert: (e messageText includesSubstring: aString) description: e messageText ].
	self assert: raised description: anErrorClass name , ' was not raised'
]

{ #category : 'fixtures' }
ZipCryptoTest >> storedArchiveBytes [
	"zip -X -P secret -0 stored.zip hello.txt, where hello.txt contains 'Hello ZipCrypto!' and a line feed"

	^ 'UEsDBAoACQAAAKd5XF34OzYkHQAAABEAAAAJAAAAaGVsbG8udHh0wyvnjGs0kpI89HNBdRl8YRUaw1hhgGRM57Z+hSZQSwcI+Ds2JB0AAAARAAAAUEsBAh4DCgAJAAAAp3lcXfg7NiQdAAAAEQAAAAkAAAAAAAAAAAAAAKSBAAAAAGhlbGxvLnR4dFBLBQYAAAAAAQABADcAAABUAAAAAAA='
		  base64Decoded
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testAESMemberRaisesClearError [
	"WinZip AES uses compression method 99"

	| member |
	member := self firstMemberOf: (self patchMethod: 99 flag: 1 in: self storedArchiveBytes).
	member password: 'secret'.
	self assert: member isStronglyEncrypted.
	self should: [ member contents ] raise: ZipArchiveError withMessageIncluding: 'AES'
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testArchivePasswordAppliesToAllMembers [

	| archive |
	archive := ZipArchive new readFrom: self deflatedArchiveBytes readStream.
	archive password: 'secret'.
	self assert: archive members first password equals: 'secret'.
	self assert: archive members first contents asString equals: self bigText
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testContentsFromToOfEncryptedMember [

	| member |
	member := self firstMemberOf: self storedArchiveBytes.
	member password: 'secret'.
	self assert: (member contentsFrom: 7 to: 15) asString equals: 'ZipCrypto'
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testCorruptedEncryptedDataRaisesCRCError [
	"Corrupt a data byte after the encryption header: the check byte still matches"

	| bytes member |
	bytes := self storedArchiveBytes.
	bytes at: 55 put: ((bytes at: 55) bitXor: 16rFF).
	member := self firstMemberOf: bytes.
	member password: 'secret'.
	self should: [ member contents ] raise: CRCError.
	self assert: member isCorrupt
]

{ #category : 'tests - cipher' }
ZipCryptoTest >> testDecryptsDataEncryptedByInfoZip [
	"The 29 data bytes of the stored fixture are the 12-byte header followed by the text"

	| plain |
	plain := (ZipCrypto password: 'secret') decrypt: (self storedArchiveBytes copyFrom: 40 to: 68).
	self assert: (plain allButFirst: 12) asString equals: 'Hello ZipCrypto!' , String lf.
	"check byte: high byte of the DOS time 16r79A7, since the member has a data descriptor"
	self assert: (plain at: 12) equals: 16r79
]

{ #category : 'tests - cipher' }
ZipCryptoTest >> testEncryptDecryptRoundTrip [

	| data cipher |
	data := (0 to: 255) asByteArray , 'some text' asByteArray.
	cipher := (ZipCrypto password: 'pässwörd') encrypt: data.
	self deny: cipher equals: data.
	self assert: cipher size equals: data size.
	self assert: ((ZipCrypto password: 'pässwörd') decrypt: cipher) equals: data.
	self deny: ((ZipCrypto password: 'password') decrypt: cipher) equals: data
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testEncryptedMemberIsCopiedAsIsWhenRewritten [
	"Without a password, an encrypted member read from an archive keeps its
	encryption and its data descriptor when the archive is written again"

	#( #storedArchiveBytes #deflatedArchiveBytes ) do: [ :fixture |
			| member rewritten |
			member := self firstMemberOf: (self perform: fixture).
			rewritten := self firstMemberOf: (self write: (ZipArchive new
						               addMember: member;
						               yourself)).
			self assert: rewritten isEncrypted.
			self assert: rewritten hasDataDescriptor.
			self assert: rewritten compressedSize equals: member compressedSize.
			rewritten password: 'secret'.
			self assert: rewritten contents equals: (member
					 password: 'secret';
					 contents) ]
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testEncryptedMemberIsCopiedAsIsWithZip64 [
	"The data descriptor copied with the member takes 8-byte sizes under Zip64"

	| member archive bytes rewritten |
	member := self firstMemberOf: self deflatedArchiveBytes.
	archive := ZipArchive new
		           addMember: member;
		           yourself.
	archive forceZip64: true.
	bytes := self write: archive.
	self assert: (bytes indexOfSubCollection: #[ 16r50 16r4B 16r07 16r08 ] startingAt: 1) > 0.
	rewritten := self firstMemberOf: bytes.
	self assert: rewritten hasDataDescriptor.
	rewritten password: 'secret'.
	self assert: rewritten contents asString equals: self bigText
]

{ #category : 'tests - cipher' }
ZipCryptoTest >> testKeysEvolveWithEachByte [
	"The same plaintext byte encrypts differently at different positions"

	| cipher |
	cipher := (ZipCrypto password: 'secret') encrypt: (ByteArray new: 16).
	self assert: cipher asSet size > 1
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testMemberWithoutPasswordIsNotEncrypted [

	| archive member |
	archive := ZipArchive new.
	archive addDeflateString: self bigText as: 'big.txt'.
	member := self firstMemberOf: (self write: archive).
	self deny: member isEncrypted.
	self assert: member contents asString equals: self bigText
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testMissingPasswordRaisesZipPasswordError [

	| member |
	member := self firstMemberOf: self storedArchiveBytes.
	self should: [ member contents ] raise: ZipPasswordError withMessageIncluding: 'is encrypted'
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testPasswordErrorIsAZipArchiveError [

	self assert: (ZipPasswordError inheritsFrom: ZipArchiveError)
]

{ #category : 'tests - cipher' }
ZipCryptoTest >> testPasswordStringIsEncodedInUtf8 [

	| data |
	data := 'abc' asByteArray.
	self assert: ((ZipCrypto password: 'é') encrypt: data) equals: ((ZipCrypto password: 'é' utf8Encoded) encrypt: data)
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testReadDeflatedMemberEncryptedByInfoZip [

	| member |
	member := self firstMemberOf: self deflatedArchiveBytes.
	self assert: member compressionMethod equals: 8.
	member password: 'secret'.
	self assert: member contents asString equals: self bigText
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testReadStoredMemberEncryptedByInfoZip [

	| member |
	member := self firstMemberOf: self storedArchiveBytes.
	self assert: member isEncrypted.
	self assert: member hasEncryptedData.
	member password: 'secret'.
	self assert: member contents asString equals: 'Hello ZipCrypto!' , String lf
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testRemovingPasswordWritesPlainMember [

	| archive member |
	archive := ZipArchive new.
	member := archive addDeflateString: self bigText as: 'big.txt'.
	member password: 'secret'.
	self write: archive.
	member password: nil.
	member := self firstMemberOf: (self write: archive).
	self deny: member isEncrypted.
	self assert: member contents asString equals: self bigText
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testStrongEncryptionRaisesClearError [
	"PKWARE strong encryption sets bit 6 together with bit 0"

	| member |
	member := self firstMemberOf: (self patchMethod: 0 flag: 2r1000001 in: self storedArchiveBytes).
	member password: 'secret'.
	self assert: member isStronglyEncrypted.
	self should: [ member contents ] raise: ZipArchiveError withMessageIncluding: 'strong encryption'
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testWriteAndReadDeflatedEncryptedMember [

	| archive bytes member |
	archive := ZipArchive new.
	(archive addDeflateString: self bigText as: 'big.txt') password: 'secret'.
	bytes := self write: archive.
	self deny: (bytes asString includesSubstring: 'pharo').
	member := self firstMemberOf: bytes.
	self assert: member isEncrypted.
	self assert: member compressionMethod equals: 8.
	self assert: member compressedSize < 100.
	member password: 'secret'.
	self assert: member contents asString equals: self bigText
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testWriteAndReadEncryptedMemberWithZip64 [

	| archive member |
	archive := ZipArchive new.
	archive forceZip64: true.
	(archive addDeflateString: self bigText as: 'big.txt') password: 'secret'.
	member := self firstMemberOf: (self write: archive).
	self assert: member isEncrypted.
	self assert: member versionNeededToExtract equals: 45.
	member password: 'secret'.
	self assert: member contents asString equals: self bigText
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testWriteAndReadStoredEncryptedMember [

	| archive bytes member |
	archive := ZipArchive new.
	member := archive addString: 'Stored secret' as: 'stored.txt'.
	member desiredCompressionMethod: 0.
	member password: 'secret'.
	bytes := self write: archive.
	self deny: (bytes asString includesSubstring: 'Stored secret').
	member := self firstMemberOf: bytes.
	self assert: member isEncrypted.
	self assert: member compressionMethod equals: 0.
	self assert: member compressedSize equals: 13 + 12.
	member password: 'secret'.
	self assert: member contents asString equals: 'Stored secret'
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testWrittenMemberRejectsWrongPassword [

	| archive member |
	archive := ZipArchive new.
	(archive addDeflateString: self bigText as: 'big.txt') password: 'secret'.
	member := self firstMemberOf: (self write: archive).
	member password: 'Secret'.
	self should: [ member contents ] raise: ZipPasswordError withMessageIncluding: 'wrong password'
]

{ #category : 'tests - writing' }
ZipCryptoTest >> testWrittenMemberUsesCrcCheckByte [
	"Pharo writes no data descriptor, so the check byte is the high byte of the CRC"

	| archive bytes member plain |
	archive := ZipArchive new.
	(archive addDeflateString: self bigText as: 'big.txt') password: 'secret'.
	bytes := self write: archive.
	member := self firstMemberOf: bytes.
	self deny: member hasDataDescriptor.
	plain := (ZipCrypto password: 'secret') decrypt: (bytes copyFrom: 30 + 7 + 1 to: 30 + 7 + 12).
	self assert: (plain at: 12) equals: (member crc32 bitShift: -24)
]

{ #category : 'tests - reading' }
ZipCryptoTest >> testWrongPasswordRaisesZipPasswordError [

	#( #storedArchiveBytes #deflatedArchiveBytes ) do: [ :fixture |
			| member |
			member := self firstMemberOf: (self perform: fixture).
			member password: 'wrong'.
			self should: [ member contents ] raise: ZipPasswordError withMessageIncluding: 'wrong password' ]
]

{ #category : 'helpers' }
ZipCryptoTest >> write: anArchive [

	^ ByteArray streamContents: [ :s | anArchive writeTo: s ]
]
