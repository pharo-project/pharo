Class {
	#name : 'ConflictMethod',
	#superclass : 'Object',
	#instVars : [
		'overwrittenSelector',
		'conflictingClass',
		'conflictingExtensions'
	],
	#category : 'Kernel-CodeModel-Extensions',
	#package : 'Kernel-CodeModel',
	#tag : 'Extensions'
}

{ #category : 'accessing' }
ConflictMethod >> conflictingClass [

	^ conflictingClass
]

{ #category : 'accessing' }
ConflictMethod >> conflictingClass: anObject [

	conflictingClass := anObject
]

{ #category : 'accessing' }
ConflictMethod >> conflictingExtensions [

	^ conflictingExtensions
]

{ #category : 'accessing' }
ConflictMethod >> conflictingExtensions: anObject [

	conflictingExtensions := anObject
]

{ #category : 'accessing' }
ConflictMethod >> extensionPackage [
	self shouldBeImplemented.
]

{ #category : 'accessing' }
ConflictMethod >> extensionPackages [
	
	^ (conflictingExtensions collect: #package) asSet
]

{ #category : 'compatibility' }
ConflictMethod >> isExtension [

	^ true
]

{ #category : 'accessing' }
ConflictMethod >> methodClass [
	
	^conflictingClass
]

{ #category : 'accessing' }
ConflictMethod >> overwrittenSelector [

	^ overwrittenSelector
]

{ #category : 'accessing' }
ConflictMethod >> overwrittenSelector: anObject [

	overwrittenSelector := anObject
]

{ #category : 'compatibility' }
ConflictMethod >> propertyAt: aKey put: aValue [

	self assert: aKey == #extensionPackage
]

{ #category : 'evaluation' }
ConflictMethod >> run: aSelector with: anArray in: aReceiver [

	ExtensionConflictError new
		messageText: ('Conflict: extension overwrites method #{1} in {2}' format: {
			overwrittenSelector asString.
			conflictingClass name asString });
		conflictingClass: conflictingClass;
		conflictingExtensions: conflictingExtensions;
		signal
]
