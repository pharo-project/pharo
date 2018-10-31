"
Test extension methods for Zip support
"
Class {
	#name : #ZipExtensionTests,
	#superclass : #TestCase,
	#category : #'Compression-Tests-Streams'
}

{ #category : #tests }
ZipExtensionTests >> testUnzippedError [
	self should: [ #[ 1 2 3 ] unzipped ] raise: Error
]

{ #category : #tests }
ZipExtensionTests >> testZippedUnzipped [
	| data |
	data := #[ 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 1 2 3 4 5 6 ].
	self assert: data zipped size < data size.
	self assert: data zipped unzipped equals: data.
	
  self assert: #[ ] zipped unzipped equals: #[ ]
]

{ #category : #tests }
ZipExtensionTests >> testZippedUnzippedString [
	| string |
	string := 'abcded abcded abcded abcded abcded abcded - 10 €'.
	self assert: string utf8Encoded zipped size < string size.
	self assert: string utf8Encoded zipped unzipped utf8Decoded equals: string
]
