"
I'm a status bar item that gets its values from a target object.
For me, #contents keeps a selector of the method that will provide the actual value. 

"
Class {
	#name : #IceTipStatusBarTargetItem,
	#superclass : #IceTipStatusBarItem,
	#instVars : [
		'target'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #accessing }
IceTipStatusBarTargetItem >> contents [
	"Contents will be a selector, here"
	^ self target perform: super contents
]

{ #category : #accessing }
IceTipStatusBarTargetItem >> help [
	"help here will be a selector, here"
	^ super help ifNotNil: [ :sel | self target perform: sel ]
]

{ #category : #accessing }
IceTipStatusBarTargetItem >> target [
	^ target
]

{ #category : #accessing }
IceTipStatusBarTargetItem >> target: anObject [
	target := anObject
]
