"
I am a button encapsulating a debug action. All my settings are set based on the corresponding debug action.
"
Class {
	#name : #SpecDebugActionButton,
	#superclass : #ButtonPresenter,
	#instVars : [
		'debugActionHolder'
	],
	#category : #Spec-Debugger
}

{ #category : #accessing }
SpecDebugActionButton >> debugAction [

	^ debugActionHolder value 
]

{ #category : #accessing }
SpecDebugActionButton >> debugAction: aDebugAction [

	debugActionHolder value: aDebugAction
]

{ #category : #initialization }
SpecDebugActionButton >> initialize [

	super initialize.

	debugActionHolder := nil asValueHolder.
	debugActionHolder whenChangedDo: [ self update ]
	
	
	
]

{ #category : #updating }
SpecDebugActionButton >> update [

	self debugAction ifNotNil:  [ :aDebugAction | 
		self label: aDebugAction label.
		self help: aDebugAction help, Character tab asString, (aDebugAction keymap ifNil: [ '' ] ifNotNil: [ :k | k   printString]).
		self icon: aDebugAction icon.
		self enabled: aDebugAction enabled.
		self state: false.
		self action: [ aDebugAction execute ] ]
]
