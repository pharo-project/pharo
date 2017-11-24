"
I am a debugging action for browsing the currently selected class.
"
Class {
	#name : #GTBrowseMethodDebuggerAction,
	#superclass : #DebugAction,
	#category : #'GT-Debugger-Actions'
}

{ #category : #registration }
GTBrowseMethodDebuggerAction class >> gtActionFor: aDebugger [
	<gtCodeDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			icon: GLMUIThemeExtraIcons glamorousBrowse
]

{ #category : #accessing }
GTBrowseMethodDebuggerAction >> defaultLabel [

	^ 'Browse'
]

{ #category : #accessing }
GTBrowseMethodDebuggerAction >> defaultOrder [

	^ 220
]

{ #category : #actions }
GTBrowseMethodDebuggerAction >> executeAction [
	Smalltalk tools browser 
	openOnClass: self debugger selectedContext methodClass
 	selector: self debugger selectedContext selector
]

{ #category : #accessing }
GTBrowseMethodDebuggerAction >> id [

	^ #gtBrowseMethodDebuggerAction
]

{ #category : #actions }
GTBrowseMethodDebuggerAction >> initialize [
	super initialize.
	self needsUpdate: false
]
