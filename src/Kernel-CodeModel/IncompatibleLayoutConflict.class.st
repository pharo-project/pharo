"
I am an error raised when extending a class with an incompatible layout.
"
Class {
	#name : 'IncompatibleLayoutConflict',
	#superclass : 'Error',
	#instVars : [
		'layout',
		'subType'
	],
	#category : 'Kernel-CodeModel-Exceptions',
	#package : 'Kernel-CodeModel',
	#tag : 'Exceptions'
}

{ #category : 'accessing' }
IncompatibleLayoutConflict >> layout [
	^ layout
]

{ #category : 'accessing' }
IncompatibleLayoutConflict >> layout: anObject [
	layout := anObject
]

{ #category : 'accessing' }
IncompatibleLayoutConflict >> messageText [
	^ 'Class with ', layout class name, ' cannot be extended as ', subType
]

{ #category : 'accessing' }
IncompatibleLayoutConflict >> subType [
	^ subType
]

{ #category : 'accessing' }
IncompatibleLayoutConflict >> subType: anObject [
	subType := anObject
]
