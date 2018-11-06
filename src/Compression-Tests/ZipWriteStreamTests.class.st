"
Unit tests for ZipWriteStream
"
Class {
	#name : #ZipWriteStreamTests,
	#superclass : #TestCase,
	#category : #'Compression-Tests-Streams'
}

{ #category : #tests }
ZipWriteStreamTests >> testEmptyStrings [
	| aStream zipStream |

	aStream := ByteArray new writeStream.
	zipStream := ZipWriteStream on: aStream.
	zipStream nextStringPut: ''. 
	zipStream nextStringPut: ''.
	zipStream close.
	aStream close.
	aStream := aStream contents readStream.
	zipStream := ZipReadStream on: aStream.
	self assert: zipStream nextString isEmpty. 
	self assert: zipStream nextString isEmpty.
	zipStream close.
	aStream close
]
