"
Test extension methods for Zip support
"
Class {
	#name : #ZipExtensionTest,
	#superclass : #TestCase,
	#category : #'Compression-Tests-Streams'
}

{ #category : #tests }
ZipExtensionTest >> testBinaryZipped [
	| data compressed |
	
	data := #[ 1 2 3 4 5 6 7 8 9 0 ].
	compressed := data zipped.
	self assert: compressed unzipped equals: data.
	
	data := #[].
	compressed := data zipped.
	self assert: compressed unzipped equals: data.
	
	data := #[ 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 ].
	compressed := data zipped.
	self assert: compressed unzipped equals: data.
	
	"next are the bytes written by 
	$ echo -n Pharo | gzip - > /tmp/foo.gz"
	compressed := #[31 139 8 0 94 254 149 93 0 3 11 200 72 44 202 7 0 144 33 178 137 5 0 0 0].
	self assert: compressed unzipped asString equals: 'Pharo'

]

{ #category : #tests }
ZipExtensionTest >> testZipped [
	| compressed |
	compressed := 'hello' zipped.
	self assert: compressed unzipped equals: 'hello'
]
