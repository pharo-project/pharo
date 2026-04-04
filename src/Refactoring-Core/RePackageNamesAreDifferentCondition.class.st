Class {
	#name : 'RePackageNamesAreDifferentCondition',
	#superclass : 'RBNewAbstractCondition',
	#instVars : [
		'packageName',
		'newName',
		'violator'
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

{ #category : 'accessing' }
RePackageNamesAreDifferentCondition >> violators [
    ^ self check
        ifFalse: [ { violator } ]
        ifTrue: [ #() ]
]
