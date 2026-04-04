Class {
	#name : 'ReTargetPackageDoesNotExistCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'packageOrganizer',
		'newName',
		'violator'
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

{ #category : 'checking' }
ReTargetPackageDoesNotExistCondition >> packageOrganizer: anOrganizer [
    packageOrganizer := anOrganizer
]

{ #category : 'displaying' }
ReTargetPackageDoesNotExistCondition >> violationMessageOn: aStream [
    aStream
    	nextPutAll: 'The system already includes a package named ';
    	nextPutAll: violator;
    	nextPut: Character cr
]

{ #category : 'accessing' }
ReTargetPackageDoesNotExistCondition >> violators [
    ^ self check
        ifFalse: [ { violator } ]
        ifTrue: [ #() ]
]
