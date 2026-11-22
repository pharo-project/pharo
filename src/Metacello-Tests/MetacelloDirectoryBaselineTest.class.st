"
A directory repository (tonel://) answers the baseline it holds now, read by Monticello, without loading it. Written on a memory file system
"
Class {
	#name : 'MetacelloDirectoryBaselineTest',
	#superclass : 'TestCase',
	#instVars : [
		'directory'
	],
	#category : 'Metacello-Tests',
	#package : 'Metacello-Tests'
}

{ #category : 'running' }
MetacelloDirectoryBaselineTest >> repository [

	^ TonelRepository new
		  directory: directory;
		  yourself
]

{ #category : 'running' }
MetacelloDirectoryBaselineTest >> setUp [

	super setUp.
	directory := FileSystem memory / 'src'.
	directory ensureCreateDirectory
]

{ #category : 'tests' }
MetacelloDirectoryBaselineTest >> testATonelDirectoryIsNotFileTree [
	"Tonel is loaded after Metacello-Core, which cannot extend it: a Tonel directory answers the default, which the FileTree warning does not take for FileTree"

	self deny: self repository fileFormat equals: #filetree
]

{ #category : 'tests' }
MetacelloDirectoryBaselineTest >> testAnswersNoBaselineItDoesNotHold [

	self assert: (self repository baselineOf: 'Foo') isNil
]

{ #category : 'tests' }
MetacelloDirectoryBaselineTest >> testReadsTheBaselineItHolds [

	self writeBaselineOfFoo.
	self
		assert: ((self repository baselineOf: 'Foo') detect: [ :each | each isMethodDefinition ]) selector
		equals: #baseline:
]

{ #category : 'running' }
MetacelloDirectoryBaselineTest >> writeBaselineOfFoo [

	| package |
	package := (directory / 'BaselineOfFoo') ensureCreateDirectory.
	(package / 'package.st') writeStreamDo: [ :stream | stream nextPutAll: 'Package { #name : #BaselineOfFoo }' ].
	(package / 'BaselineOfFoo.class.st') writeStreamDo: [ :stream |
			stream nextPutAll: 'Class {
	#name : #BaselineOfFoo,
	#superclass : #BaselineOf,
	#category : #BaselineOfFoo
}

{ #category : #baselines }
BaselineOfFoo >> baseline: spec [
	<baseline>
	spec for: #common do: [ spec package: #Foo ]
]
' ]
]
