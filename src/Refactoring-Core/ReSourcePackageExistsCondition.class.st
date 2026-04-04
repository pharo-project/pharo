Class {
	#name : 'ReSourcePackageExistsCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'packageName',
		'packageOrganizer',
		'violator'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
ReSourcePackageExistsCondition >> check [
    (packageOrganizer hasPackage: packageName) ifFalse: [
        violator := packageName.
        ^ false ].
    ^ true
]

{ #category : 'checking' }
ReSourcePackageExistsCondition >> packageName: aName [
    packageName := aName
]

{ #category : 'checking' }
ReSourcePackageExistsCondition >> packageOrganizer: anOrganizer [
    packageOrganizer := anOrganizer
]

{ #category : 'checking' }
ReSourcePackageExistsCondition >> violationMessageOn: aStream [
    aStream
        nextPutAll: 'The package ';
        nextPutAll: violator;
        nextPutAll: ' does not exist in the system';
        nextPut: Character cr
]

{ #category : 'accessing' }
ReSourcePackageExistsCondition >> violators [
    ^ self check
        ifFalse: [ { violator } ]
        ifTrue: [ #() ]
]
