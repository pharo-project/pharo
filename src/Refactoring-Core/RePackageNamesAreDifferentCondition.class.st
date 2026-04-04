Class {
	#name : 'RePackageNamesAreDifferentCondition',
	#superclass : 'RePackageCondition',
	#instVars : [
		'packageName',
		'newName'
	],
	#category : 'Refactoring-Core-Conditions',
	#package : 'Refactoring-Core',
	#tag : 'Conditions'
}

{ #category : 'checking' }
RePackageNamesAreDifferentCondition >> check [
    newName = packageName ifTrue: [
        violator := newName.
        ^ false ].
    ^ true
]

{ #category : 'accessing' }
RePackageNamesAreDifferentCondition >> newName: aName [
    newName := aName
]

{ #category : 'accessing' }
RePackageNamesAreDifferentCondition >> packageName: aName [
    packageName := aName
]

{ #category : 'displaying' }
RePackageNamesAreDifferentCondition >> violationMessageOn: aStream [
    aStream
        nextPutAll: 'The new package name is the same as the old package name.';
        nextPut: Character cr
]
