Class {
	#name : #ZipExtensionTests,
	#superclass : #TestCase,
	#category : #Compression-Tests-Streams
}

{ #category : #tests }
ZipExtensionTests >> testZipped [
	| compressed |
	
	compressed := 'hello' zipped.
	self assert: (compressed unzipped = 'hello').
]
