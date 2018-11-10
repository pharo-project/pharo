"
I'm a base class for merge types. 
My children define strategies of how to merge two commitish.
	

"
Class {
	#name : #IceTipMergeType,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #accessing }
IceTipMergeType class >> default [
	^ self direct
]

{ #category : #accessing }
IceTipMergeType class >> direct [
	^ IceTipDirectMergeType uniqueInstance
]

{ #category : #'instance creation' }
IceTipMergeType class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #accessing }
IceTipMergeType class >> switchAndMerge [
	^ IceTipSwitchAndMergeType uniqueInstance
]

{ #category : #'instance creation' }
IceTipMergeType class >> uniqueInstance [ 
	self = IceTipMergeType 
		ifTrue: [ self error: 'I''m an abstract type. Use one of my children' ].
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #executing }
IceTipMergeType >> mergeOn: aModel then: aBlock [
	self subclassResponsibility
]
