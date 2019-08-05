"
This class is a simple data structure to be used for identifying a port by name.
"
Class {
	#name : #GLMPortIdentifier,
	#superclass : #Object,
	#instVars : [
		'paneName',
		'portName'
	],
	#category : #'Glamour-Helpers'
}

{ #category : #'instance creation' }
GLMPortIdentifier class >> defaultOriginOf: aPaneName [
	^ self new 
		paneName: aPaneName;
		portName: self defaultOriginPortName
]

{ #category : #accessing }
GLMPortIdentifier class >> defaultOriginPortName [
	^ #selection
]

{ #category : #'instance creation' }
GLMPortIdentifier class >> defaultTargetOf: aPaneName [
	^ self new 
		paneName: aPaneName;
		portName: self defaultTargetPortName
]

{ #category : #accessing }
GLMPortIdentifier class >> defaultTargetPortName [
	^ #entity
]

{ #category : #'instance creation' }
GLMPortIdentifier class >> pane: aPaneName port: aPortName [
	^ self new 
		paneName: aPaneName;
		portName: aPortName
]

{ #category : #accessing }
GLMPortIdentifier >> asGlamourOriginIdentifier [

	self portName ifNil: [ self portName: self class defaultOriginPortName ].
	^ self
]

{ #category : #accessing }
GLMPortIdentifier >> asGlamourTargetIdentifier [

	self portName ifNil: [ self portName: self class defaultTargetPortName ].
	^ self
]

{ #category : #accessing }
GLMPortIdentifier >> paneName [
	^ paneName
]

{ #category : #accessing }
GLMPortIdentifier >> paneName: anObject [
	paneName := anObject
]

{ #category : #accessing }
GLMPortIdentifier >> portName [
	^ portName
]

{ #category : #accessing }
GLMPortIdentifier >> portName: anObject [
	portName := anObject
]
