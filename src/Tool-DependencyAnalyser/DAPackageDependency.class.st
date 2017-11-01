"
A PDPackageDependency is a dependency between two packages.

	source:		the source PDPackage 
	target:	      the target PDPackage

"
Class {
	#name : #DAPackageDependency,
	#superclass : #Object,
	#instVars : [
		'source',
		'target'
	],
	#category : 'Tool-DependencyAnalyser-Core'
}

{ #category : #instance-creation }
DAPackageDependency class >> from: aSourcePackage to: aDestinationPackage [
	^ self new initializeFrom: aSourcePackage to: aDestinationPackage
]

{ #category : #printing }
DAPackageDependency >> displayString [
	^ String streamContents: [ :stream | self printReasonOn: stream ]
]

{ #category : #comparing }
DAPackageDependency >> hash [
	^ source hash bitXor: target hash
]

{ #category : #initialization }
DAPackageDependency >> initializeFrom: aSourcePackage to: aTargetPackage [
	source := aSourcePackage.
	target := aTargetPackage
]

{ #category : #testing }
DAPackageDependency >> isExtensionDependency [
	^ false
]

{ #category : #testing }
DAPackageDependency >> isExternal [
	^ self isInternal not
]

{ #category : #testing }
DAPackageDependency >> isInheritanceDependency [
	^ false 
]

{ #category : #testing }
DAPackageDependency >> isInternal [
	^ self source = self target
]

{ #category : #testing }
DAPackageDependency >> isMessageSendDependency [
	^ false 
]

{ #category : #testing }
DAPackageDependency >> isReferenceDependency [
	^ false
]

{ #category : #testing }
DAPackageDependency >> isTraitDependency [
	^ false
]

{ #category : #accessing }
DAPackageDependency >> numberOfDependencies [
	^ 1
]

{ #category : #accessing }
DAPackageDependency >> packageName [
	^ target packageName
]

{ #category : #printing }
DAPackageDependency >> printOn: aStream [
	self printReasonOn: aStream
]

{ #category : #printing }
DAPackageDependency >> printReasonOn: aStream [
	aStream 
		nextPutAll: source packageName;
		nextPutAll: ' -> ';
		nextPutAll: target packageName
]

{ #category : #printing }
DAPackageDependency >> printShortReasonOn: aStream [
	self printReasonOn: aStream
]

{ #category : #accessing }
DAPackageDependency >> reason [
	^ String streamContents: [ :stream | self printReasonOn: stream ]
]

{ #category : #accessing }
DAPackageDependency >> source [
	^ source
]

{ #category : #accessing }
DAPackageDependency >> target [
	^ target
]
