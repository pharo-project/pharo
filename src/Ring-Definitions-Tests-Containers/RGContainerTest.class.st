"
SUnit tests for container
"
Class {
	#name : #RGContainerTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Containers-Base'
}

{ #category : #tests }
RGContainerTest >> testCategoryMatching [

	self assert: ('Compression-Archives' isCategoryOf: 'Compression').
	self deny: ('Compression' isCategoryOf: 'Compression-Archives').
	self assert: ('Compression' isCategoryOf: 'Compression').
	self assert: ('Compression-Arh' isCategoryOf: 'Compression').
	self deny: ('XXXX' isCategoryOf: 'Compression-Archives')
]
