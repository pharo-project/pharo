"
I abstract file creation so that I can track all the files that are created by tests and can delete them after the tests have ended.
"
Class {
	#name : 'FLFilePoolTestResource',
	#superclass : 'TestResource',
	#instVars : [
		'directoryPath'
	],
	#category : 'Fuel-Core-Tests-Support-Resources',
	#package : 'Fuel-Core-Tests',
	#tag : 'Support-Resources'
}

{ #category : 'testing' }
FLFilePoolTestResource >> fileExists: aString [
	^ (self filePathString: aString) asFileReference exists
]

{ #category : 'accessing-files' }
FLFilePoolTestResource >> filePathString: aString [
	^ (directoryPath asFileReference / aString) pathString
]

{ #category : 'accessing-files' }
FLFilePoolTestResource >> readStreamForFilePath: aString [
	^ (File named: (self filePathString: aString)) readStream
		  binary;
		  yourself
]

{ #category : 'running' }
FLFilePoolTestResource >> setUp [
	super setUp.
	
	directoryPath := (FileLocator temp / UUID new asString) ensureCreateDirectory pathString
]

{ #category : 'running' }
FLFilePoolTestResource >> tearDown [
	directoryPath asFileReference deleteAll.

	super tearDown
]

{ #category : 'accessing-files' }
FLFilePoolTestResource >> writeStreamForFilePath: aString [
	^ (File named: (self filePathString: aString)) writeStream
		  binary;
		  yourself
]
