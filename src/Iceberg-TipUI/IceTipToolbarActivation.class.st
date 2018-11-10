Class {
	#name : #IceTipToolbarActivation,
	#superclass : #CmdMenuCommandActivationStrategy,
	#instVars : [
		'position'
	],
	#category : #'Iceberg-TipUI-Spec-ItemBar'
}

{ #category : #'instance creation' }
IceTipToolbarActivation class >> byRootGroupItemFor: aToolContextClass order: aNumber [
	^ (self byRootGroupItemFor: aToolContextClass)	
		menuItemOrder: aNumber;
		yourself
]

{ #category : #accessing }
IceTipToolbarActivation >> help [
	^ self commandClass defaultHelp
]

{ #category : #accessing }
IceTipToolbarActivation >> position [
	^ position ifNil: [ IceTipItemPosition perform: self commandClass defaultPosition ]
]

{ #category : #accessing }
IceTipToolbarActivation >> position: aPosition [
	"one of IceTipToolbarItemPosition" 
	position := aPosition
]

{ #category : #accessing }
IceTipToolbarActivation >> toolbarName [
	^ self commandClass defaultToolbarItemName
]
