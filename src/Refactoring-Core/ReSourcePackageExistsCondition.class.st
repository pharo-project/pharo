Class {
	#name : 'ReSourcePackageExistsCondition',
	#superclass : 'RePackageCondition',
	#instVars : [
		'packageName'
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
ReSourcePackageExistsCondition >> violationMessageOn: aStream [
    aStream
        nextPutAll: 'The package ';
        nextPutAll: violator;
        nextPutAll: ' does not exist in the system';
        nextPut: Character cr
]
