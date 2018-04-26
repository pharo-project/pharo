"
SUnit tests for MD5
"
Class {
	#name : #MD5Test,
	#superclass : #ClassTestCase,
	#instVars : [
		'hash'
	],
	#category : #'System-Hashing-Tests-MD5'
}

{ #category : #testing }
MD5Test >> testEmpty [
	
	self 
		assert: (MD5 hashMessage: '') hex 
		equals: 'd41d8cd98f00b204e9800998ecf8427e'
]

{ #category : #testing }
MD5Test >> testLargeCharacterStream [
	"Regression test"

	| string |
	string := String streamContents: [ :out | 10 timesRepeat: [ out << Character alphabet ] ].
	hash := MD5 hashStream: string readStream.
	self assert: hash hex equals: '4e6405697346169610a3a39991c48321'
]

{ #category : #testing }
MD5Test >> testWikipedia1 [
	self 
		assert: (MD5 hashMessage: 'The quick brown fox jumps over the lazy dog') hex 
		equals: '9e107d9d372bb6826bd81d3542a419d6'
]

{ #category : #testing }
MD5Test >> testWikipedia2 [
	self 
		assert: (MD5 hashMessage: 'The quick brown fox jumps over the lazy dog.') hex 
		equals: 'e4d909c290d0fb1ca068ffaddf22cbd0'
]
