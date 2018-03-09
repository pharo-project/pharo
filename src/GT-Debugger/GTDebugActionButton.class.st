Class {
	#name : #GTDebugActionButton,
	#superclass : #ButtonPresenter,
	#instVars : [
		'debugActionHolder'
	],
	#category : #'GT-Debugger-Model'
}

{ #category : #accessing }
GTDebugActionButton >> debugAction [

	^ debugActionHolder value 
]

{ #category : #accessing }
GTDebugActionButton >> debugAction: aDebugAction [

	debugActionHolder value: aDebugAction
]

{ #category : #initialization }
GTDebugActionButton >> initialize [

	super initialize.

	debugActionHolder := nil asValueHolder.
	debugActionHolder whenChangedDo: [ self update ]
	
	
	
]

{ #category : #updating }
GTDebugActionButton >> update [

	self debugAction ifNotNil:  [ :aDebugAction | 
		self label: aDebugAction label.
		self help: aDebugAction help, Character tab asString, (aDebugAction keymap ifNil: [ '' ] ifNotNil: [ :k | k   printString]).
		self icon: aDebugAction icon.
		self enabled: aDebugAction enabled.
		self state: false.
		self action: [ aDebugAction execute ] ]
]
