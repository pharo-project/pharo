Class {
	#name : 'ExtensionConflictError',
	#superclass : 'Error',
	#instVars : [
		'selector',
		'conflictingClass',
		'conflictingExtensions'
	],
	#category : 'Kernel-CodeModel',
	#package : 'Kernel-CodeModel'
}

{ #category : 'accessing' }
ExtensionConflictError >> conflictingClass [

	^ conflictingClass
]

{ #category : 'accessing' }
ExtensionConflictError >> conflictingClass: aConflictingClass [

	conflictingClass := aConflictingClass
]

{ #category : 'accessing' }
ExtensionConflictError >> conflictingExtensions [

	^ conflictingExtensions
]

{ #category : 'accessing' }
ExtensionConflictError >> conflictingExtensions: anObject [

	conflictingExtensions := anObject
]

{ #category : 'initialization' }
ExtensionConflictError >> initialize [

	super initialize.
	conflictingExtensions := #()
]

{ #category : 'accessing' }
ExtensionConflictError >> selector [

	^ selector
]

{ #category : 'accessing' }
ExtensionConflictError >> selector: aSelector [

	selector := aSelector
]
