"
I am an OSWindow world morph that is used by Glamour to display its content in a external OSWindow.

Usage examples:

GTInspector new openInExternalWindowOn: 3.

3 inspectInExternalWindow.
"
Class {
	#name : #GLMOSWindowWorldMorph,
	#superclass : #OSWindowWorldMorphWithModel,
	#instVars : [
		'toolbarBox'
	],
	#category : #OSWindow-Core-Morphic
}

{ #category : #callbacks }
GLMOSWindowWorldMorph >> actOnWindowRequest: ann [

	ann action value: self	
]

{ #category : #submorphs-add/remove }
GLMOSWindowWorldMorph >> delete [
	self model notNil ifTrue: [self model unregisterFromAllAnnouncements].
	super delete
]

{ #category : #initialization }
GLMOSWindowWorldMorph >> initialize [
	super initialize.
	
	self layoutPolicy: ProportionalLayout new.
	self clipSubmorphs: true.

]

{ #category : #accessing }
GLMOSWindowWorldMorph >> toolbarBox: anObject [
	toolbarBox := anObject
]

{ #category : #announcing }
GLMOSWindowWorldMorph >> when: anAnnouncement do: aBlock [ 
	^ self announcer when: anAnnouncement do: aBlock
]
