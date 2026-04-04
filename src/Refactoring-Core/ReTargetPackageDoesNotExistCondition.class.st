Class {
	#name : 'ReTargetPackageDoesNotExistCondition',
	#superclass : 'RePackageCondition',
	#instVars : [
		'newName'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReTargetPackageDoesNotExistCondition >> check [
    (packageOrganizer hasPackage: newName) ifTrue: [
        violator := newName.
        ^ false ].
    ^ true
]

{ #category : 'accessing' }
ReTargetPackageDoesNotExistCondition >> newName: aName [
    newName := aName
]

{ #category : 'displaying' }
ReTargetPackageDoesNotExistCondition >> violationMessageOn: aStream [
    aStream
    	nextPutAll: 'The system already includes a package named ';
    	nextPutAll: violator;
    	nextPut: Character cr
]
