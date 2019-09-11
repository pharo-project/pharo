"
I am a base class for status bar items related to particular browser tab (tool).
My subclasses should implement method #build where they should create required widgets and subscribe of required events.

My instance should be create for concrete browser tool: 

	ClyStatusBarItem for: aBrowserTool
	
Internal Representation and Key Implementation Points.

    Instance Variables
	ownerTool:		<ClyBrowserTool>
"
Class {
	#name : #ClyStatusBarItemMorph,
	#superclass : #Morph,
	#instVars : [
		'ownerTool'
	],
	#category : #'Calypso-Browser-UI'
}

{ #category : #'instance creation' }
ClyStatusBarItemMorph class >> for: aBrowserTool [
	^self new 
		ownerTool: aBrowserTool 
]

{ #category : #building }
ClyStatusBarItemMorph >> build [
	self subclassResponsibility 
]

{ #category : #initialization }
ClyStatusBarItemMorph >> defaultColor [
	^Color transparent
]

{ #category : #initialization }
ClyStatusBarItemMorph >> initialize [
	super initialize.	
	self 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #shrinkWrap;
		extent: 0@0
]

{ #category : #accessing }
ClyStatusBarItemMorph >> ownerTool [
	^ ownerTool
]

{ #category : #accessing }
ClyStatusBarItemMorph >> ownerTool: anObject [
	ownerTool := anObject.
	self build
]
