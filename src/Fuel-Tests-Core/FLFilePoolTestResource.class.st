Class {
	#name : #FLFilePoolTestResource,
	#superclass : #TestResource,
	#instVars : [
		#directoryPath
	],
	#category : #'Fuel-Tests-Core-Support-Resources'
}

{ #category : #testing }
FLFilePoolTestResource >> fileExists: aString [
	^ FLPlatform current filePathExists: (self filePathString: aString)
]

{ #category : #'accessing-files' }
FLFilePoolTestResource >> filePathString: aString [
	^ FLPlatform current
		joinPath: directoryPath
		with: aString
]

{ #category : #'accessing-files' }
FLFilePoolTestResource >> readStreamForFilePath: aString [
	^ FLPlatform current readStreamForFilePath: (self filePathString: aString)
]

{ #category : #running }
FLFilePoolTestResource >> setUp [
	super setUp.
	
	directoryPath := FLPlatform current createTemporaryDirectoryPathNamed: UUID new asString
]

{ #category : #running }
FLFilePoolTestResource >> tearDown [
	FLPlatform current deleteAllInDirectoryPath: directoryPath.
	
	super tearDown
]

{ #category : #'accessing-files' }
FLFilePoolTestResource >> writeStreamForFilePath: aString [
	^ FLPlatform current writeStreamForFilePath: (self filePathString: aString)
]
