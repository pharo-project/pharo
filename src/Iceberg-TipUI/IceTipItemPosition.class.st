"
I'm a position in a toolbar.
My children define where IceTipToolbarItem instance will be located. 
"
Class {
	#name : #IceTipItemPosition,
	#superclass : #Object,
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipItemPosition class >> default [
	^ self left	
]

{ #category : #accessing }
IceTipItemPosition class >> left [
	^ IceTipItemPositionLeft uniqueInstance
	
]

{ #category : #'instance creation' }
IceTipItemPosition class >> new [
	self error: 'Use uniqueInstance'
]

{ #category : #accessing }
IceTipItemPosition class >> right [
	^ IceTipItemPositionRight uniqueInstance
	
]

{ #category : #'instance creation' }
IceTipItemPosition class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #accessing }
IceTipItemPosition >> addItem: anObject into: aToolbar [
	self subclassResponsibility
]
