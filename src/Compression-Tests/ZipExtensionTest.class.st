"
Test extension methods for Zip support
"
Class {
	#name : #ZipExtensionTest,
	#superclass : #TestCase,
	#category : #'Compression-Tests-Streams'
}

{ #category : #tests }
ZipExtensionTest >> testZipped [
	| compressed |
	compressed := 'hello' zipped.
	self assert: compressed unzipped equals: 'hello'
]
