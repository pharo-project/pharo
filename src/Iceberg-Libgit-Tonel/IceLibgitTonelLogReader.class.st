"
I read exclusively the definition of the method defined in fileName.
I'm using to speed the creation of a version list on IceLibgitTonelLog.
"
Class {
	#name : #IceLibgitTonelLogReader,
	#superclass : #IceLibgitTonelReader,
	#instVars : [
		'fileName'
	],
	#category : #'Iceberg-Libgit-Tonel'
}

{ #category : #'instance creation' }
IceLibgitTonelLogReader class >> fileName: aString on: aCommit [
	^ (self on: aCommit)
		fileName: aString;
		yourself
]

{ #category : #private }
IceLibgitTonelLogReader >> canBeLoaded: aFileReference [
	^ (super canBeLoaded: aFileReference)
		and: [ aFileReference filename = self fileName ] 
]

{ #category : #accessing }
IceLibgitTonelLogReader >> fileName [
	^ fileName
]

{ #category : #accessing }
IceLibgitTonelLogReader >> fileName: aString [
	fileName := aString
]

{ #category : #accessing }
IceLibgitTonelLogReader >> package [
	^ package
]

{ #category : #accessing }
IceLibgitTonelLogReader >> package: aPackage [
	package := aPackage
]
