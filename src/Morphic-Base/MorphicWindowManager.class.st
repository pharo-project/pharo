"
I manage the native window, When I am created I open a window with a taskbar.
If you create a window, the window is add at the taskbar.


    Instance Variables
	allWindow:		<Object>
	allWindowState:		<Object>
	allWorld:		<Object>
	taskBar:		<Object>
	taskBarWindow:		<Object>


    Implementation Points
"
Class {
	#name : #MorphicWindowManager,
	#superclass : #Morph,
	#instVars : [
		'taskBar',
		'taskBarWindow',
		'rootSave',
		'allAttributes',
		'windows'
	],
	#category : #'Morphic-Base'
}

{ #category : #accessing }
MorphicWindowManager >> allAttributes [

	^ allAttributes
]

{ #category : #accessing }
MorphicWindowManager >> allSaveRoot [

	^ rootSave
]

{ #category : #accessing }
MorphicWindowManager >> allWindow [ 

	^ windows 
]

{ #category : #accessing }
MorphicWindowManager >> closeAllWindowToLeft: aWindow [

	| index |
	index := windows indexOf: aWindow.
	index - 1 to: 1 by: -1 do: [ :m | (windows at: m) delete ].
	taskBar updateTasks
]

{ #category : #accessing }
MorphicWindowManager >> closeAllWindowToRight: aWindow [

	| index |
	index := windows indexOf: aWindow.
	windows size to: index + 1 by: -1 do: [ :m | 
	(windows at: m) delete ].
	taskBar updateTasks
]

{ #category : #accessing }
MorphicWindowManager >> closeAllWindows [
	"I delete all the windows that are attached to me starting from the end."

	windows size to: 1 by: -1 do: [ :m | (windows at: m) delete ].
	taskBar updateTasks
]

{ #category : #accessing }
MorphicWindowManager >> closeWindow: aWindow [
	"If we don't have the window, they do Nothing"

	(windows includes: aWindow) ifTrue: [ windows remove: aWindow ].
	taskBar ifNotNil: [ taskBar updateTasks ]
]

{ #category : #closing }
MorphicWindowManager >> deleteAll [

	self closeAllWindows.

	"Call doDelete because the taskbar window is not subscribed to manager"
	taskBarWindow doDelete
]

{ #category : #accessing }
MorphicWindowManager >> ensureCloseAllWindows [

	windows size to: 1 by: -1 do: [ :m | 
		(windows at: m) ensureDelete ].
	taskBar updateTasks
]

{ #category : #closing }
MorphicWindowManager >> ensureDeleteAll [

	self ensureCloseAllWindows.

	"Call ensureDelete because the taskbar window is not subscribed to manager"
	taskBarWindow ensureDelete  
]

{ #category : #initialization }
MorphicWindowManager >> initialize [

	windows := OrderedCollection new.
	self startupTaskbar
]

{ #category : #testing }
MorphicWindowManager >> isTaskBarOpen [
	
	^ taskBarWindow isOpen
]

{ #category : #accessing }
MorphicWindowManager >> newWindowWithRoot: aMorph [

	| newWindow |
	newWindow := MorphicNativeWindow newWindow: aMorph manageBy: self.
	windows add: newWindow.
	^ newWindow
]

{ #category : #size }
MorphicWindowManager >> numberOfAttributesSave [
	
	^ allAttributes size
]

{ #category : #accessing }
MorphicWindowManager >> numberOfOpenWindows [

	^ windows size
]

{ #category : #accessing }
MorphicWindowManager >> numberOfRootSave [
	
	^ rootSave size 
]

{ #category : #accessing }
MorphicWindowManager >> numberOfWindows [

	^ windows size
]

{ #category : #accessing }
MorphicWindowManager >> openNewWindowWithRoot: aMorph [

	| newWindow |
	newWindow := (self newWindowWithRoot: aMorph) open.
	taskBar updateTasks.
	^ newWindow
]

{ #category : #saving }
MorphicWindowManager >> saveAllAttributes [
	
	allAttributes := windows collect: [  :w | w attributesSave ].
]

{ #category : #saving }
MorphicWindowManager >> saveAllRoot [
	
	rootSave := windows collect: [ :m | m window ]
]

{ #category : #'startup - shutdown' }
MorphicWindowManager >> startupRoot [
	
	1 to: rootSave size do: [ :r | self newWindowWithRoot:  (rootSave at: r) ]
]

{ #category : #startup }
MorphicWindowManager >> startupTaskbar [

	taskBarWindow := MorphicNativeWindow
		                 newWindow:
		                 (TaskbarMorphForManager newByManager: self)
		                 manageBy: self.
	taskBarWindow open.
	taskBar := taskBarWindow window
]

{ #category : #accessing }
MorphicWindowManager >> taskBar [

	^ taskBar 
]
