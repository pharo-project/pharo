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
	#name : 'MorphicWindowManager',
	#superclass : 'Morph',
	#instVars : [
		'taskBar',
		'taskBarWindow',
		'rootSave',
		'allAttributes',
		'windows'
	],
	#category : 'Morphic-Base',
	#package : 'Morphic-Base'
}

{ #category : 'instance creation' }
MorphicWindowManager class >> open [

	<script>
	| manager |
	manager := self new.
	manager open.
	^ manager
]

{ #category : 'accessing' }
MorphicWindowManager >> allAttributes [

	^ allAttributes
]

{ #category : 'accessing' }
MorphicWindowManager >> allSaveRoot [

	^ rootSave
]

{ #category : 'accessing' }
MorphicWindowManager >> allWindow [ 

	^ windows 
]

{ #category : 'accessing' }
MorphicWindowManager >> closeAllWindowToLeft: aWindow [

	| index |
	index := windows indexOf: aWindow.
	index - 1 to: 1 by: -1 do: [ :m | (windows at: m) delete ].
	self updateTasksBar
]

{ #category : 'accessing' }
MorphicWindowManager >> closeAllWindowToRight: aWindow [

	| index |
	index := windows indexOf: aWindow.
	windows size to: index + 1 by: -1 do: [ :m | (windows at: m) delete ].
	self updateTasksBar
]

{ #category : 'accessing' }
MorphicWindowManager >> closeAllWindows [
	"I delete all the windows opened that are attached to me starting from the end."

	windows size to: 1 by: -1 do: [ :m | (windows at: m) delete ].
	self updateTasksBar
]

{ #category : 'accessing' }
MorphicWindowManager >> closeWindow: aWindow [
	"If we don't have the window, they do Nothing"

	(windows includes: aWindow) ifTrue: [ windows remove: aWindow ].
	((aWindow hash)= ( taskBarWindow hash) )ifFalse: [self updateTasksBar]
]

{ #category : 'closing' }
MorphicWindowManager >> deleteAll [

	self closeAllWindows.

	"Call doDelete because the taskbar window is not subscribed to manager"
	taskBarWindow ifNotNil: [ taskBarWindow doDelete ]
]

{ #category : 'accessing' }
MorphicWindowManager >> ensureCloseAllWindow [

	windows size to: 1 by: -1 do: [ :m | (windows at: m) ensureDelete ].
	self updateTasksBar
]

{ #category : 'accessing' }
MorphicWindowManager >> ensureCloseAllWindows [

	windows size to: 1 by: -1 do: [ :m | (windows at: m) ensureDelete ].
	self updateTasksBar
]

{ #category : 'closing' }
MorphicWindowManager >> ensureDeleteAll [

	self ensureCloseAllWindows.

	"Call ensureDelete because the taskbar window is not subscribed to manager"
	taskBarWindow ifNotNil: [ taskBarWindow ensureDelete ]
]

{ #category : 'initialization' }
MorphicWindowManager >> initialize [

	windows := OrderedCollection new.

]

{ #category : 'testing' }
MorphicWindowManager >> isTaskBarOpen [
	
	^ taskBarWindow isOpen
]

{ #category : 'accessing' }
MorphicWindowManager >> newWindowWithRoot: aMorph [

	| newWindow |
	newWindow := MorphicNativeWindow newWindow: aMorph manageBy: self.
	windows add: newWindow.
	^ newWindow
]

{ #category : 'size' }
MorphicWindowManager >> numberOfAttributesSave [
	
	^ allAttributes size
]

{ #category : 'accessing' }
MorphicWindowManager >> numberOfOpenWindows [

	^ windows size
]

{ #category : 'accessing' }
MorphicWindowManager >> numberOfRootSave [
	
	^ rootSave size 
]

{ #category : 'accessing' }
MorphicWindowManager >> numberOfWindows [

	^ windows size
]

{ #category : 'instance creation' }
MorphicWindowManager >> open [

	self startupTaskbar
]

{ #category : 'accessing' }
MorphicWindowManager >> openNewWindowWithRoot: aMorph [

	| newWindow |
	newWindow := (self newWindowWithRoot: aMorph) open.

	self updateTasksBar.
	^ newWindow
]

{ #category : 'saving' }
MorphicWindowManager >> saveAllAttributes [
	
	allAttributes := windows collect: [  :w | w attributsSave ].
]

{ #category : 'saving' }
MorphicWindowManager >> saveAllRoot [
	
	rootSave := windows collect: [ :m | m window ]
]

{ #category : 'startup - shutdown' }
MorphicWindowManager >> startupRoot [
	
	1 to: rootSave size do: [ :r | self newWindowWithRoot:  (rootSave at: r) ]
]

{ #category : 'startup' }
MorphicWindowManager >> startupTaskbar [

	taskBarWindow := MorphicNativeWindow
		                 newWindow:
		                 (TaskbarMorphForManager newByManager: self)
		                 manageBy: self.
	taskBarWindow open.
	taskBar := taskBarWindow window
]

{ #category : 'accessing' }
MorphicWindowManager >> taskBar [

	^ taskBar 
]

{ #category : 'update' }
MorphicWindowManager >> updateTasksBar [

	taskBarWindow ifNotNil: [taskBar ifNotNil: [ taskBar updateTasks ]]
]
