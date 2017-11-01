"
Code that is not contained in a Monticello package is not versioned and cannot be brought into a different image.
"
Class {
	#name : #RBUnpackagedCodeRule,
	#superclass : #RBBlockLintRule,
	#instVars : [
		'packages',
		'package'
	],
	#category : 'Refactoring-Critics-BlockRules'
}

{ #category : #testing }
RBUnpackagedCodeRule class >> checksClass [
	^ true
]

{ #category : #testing }
RBUnpackagedCodeRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBUnpackagedCodeRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UnpackagedCodeRule'
]

{ #category : #running }
RBUnpackagedCodeRule >> basicCheck: anEntity [

	^ anEntity package ifNil: [ true ] ifNotNil: #isDefault
]

{ #category : #running }
RBUnpackagedCodeRule >> checkClass: aClass [ 
	(self basicCheck: aClass) ifTrue: [
		self result addClass: aClass ]
]

{ #category : #running }
RBUnpackagedCodeRule >> checkMethod: aMethod [
	aMethod isDoIt ifTrue: [ ^self ].
	(self basicCheck: aMethod) ifTrue: [
		self result addMethod: aMethod ]
]

{ #category : #accessing }
RBUnpackagedCodeRule >> group [
	^ 'Potential Bugs'
]

{ #category : #initialization }
RBUnpackagedCodeRule >> initialize [
	super initialize.
	packages := MCWorkingCopy allManagers
		inject: #()
		into: [ :all :each | all, (each packageSet packages) ]
]

{ #category : #accessing }
RBUnpackagedCodeRule >> name [
	^ 'Unpackaged code'
]

{ #category : #private }
RBUnpackagedCodeRule >> packageSatisfying: aBlock [
	"Answer the first package satisfying aBlock or nil. This method assumes that it is likely that the last matching package matches the given condition again and thus it tries that one first."

	(package notNil and: [ aBlock value: package ])
		ifTrue: [ ^ package ].
	packages do: [ :info |
		(aBlock value: info)
			ifTrue: [ ^ package := info ] ].
	^ nil
]
