"
I represent a native window

I open a new window with a specific backend, i know my manager, i have all atributs for the window.

For the Collaborators Part: on github: @RemiDufloer

Internal Representation and Key Implementation Points.

    Instance Variables
	attributs:		the attributs of the window (title ect..)
	backend:		sdl or gtk
	manager:		aWindowManager
	osWindow:		osWindow
	window:		aMorph
			
"
Class {
	#name : 'MorphicNativeWindow',
	#superclass : 'Object',
	#traits : 'TShowInTaskbar',
	#classTraits : 'TShowInTaskbar classTrait',
	#instVars : [
		'window',
		'backend',
		'manager',
		'osWindow',
		'attributes'
	],
	#category : 'Morphic-Base',
	#package : 'Morphic-Base'
}

{ #category : 'instance creation' }
MorphicNativeWindow class >> newWindow: aMorph manageBy: aManager [

	| newWindow |
	newWindow  := self new.
	newWindow window: aMorph.
	newWindow manager: aManager.
	^ newWindow 
	
]

{ #category : 'closing' }
MorphicNativeWindow >> allowedToClose [

" no reason to dont close ? "
	^ true
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> attributs [

	^ attributes
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> attributs: aOsWindowAttributs [

	attributes := aOsWindowAttributs
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> attributsSave [

	osWindow position.
	osWindow title.
	osWindow bounds.
	
	^ attributes 
]

{ #category : 'accessing' }
MorphicNativeWindow >> backend [

	^ backend
]

{ #category : 'testing' }
MorphicNativeWindow >> canBeMaximized [
	"Answer whether we are not we can be maximised."

	^ attributes  resizable  and: [ attributes maximized not ]
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> closeAllWindowToLeft [

	manager closeAllWindowToLeft: self
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> closeAllWindowToRight [

	manager closeAllWindowToRight: self
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> delete [

	self doDelete.
	manager closeWindow: self
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> deleteByCross [ 

	manager ifNotNil: [ manager closeWindow: self ] 
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> doDelete [

	"Low-level deletion method.
	Do not use directly, otherwise the connection with the manager would be lost.

	Use #delete instead"

	self isOpen ifFalse: [ self error: 'Cannot delete non open window' ].
	self ensureDoDelete
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> ensureDelete [

	self ensureDoDelete.
	manager closeWindow: self
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> ensureDoDelete [

	"Low-level deletion method.
	Do not use directly, otherwise the connection with the manager would be lost.

	Use #ensureDelete instead.
	
	Do not fail if the window is not open"

	self isOpen ifFalse: [ ^ self ].

	"osWindow destroy ."
	window delete.
	
	manager closeWindow: self.
]

{ #category : 'testing' }
MorphicNativeWindow >> isClosed [
	
	^ self isOpen not
]

{ #category : 'testing' }
MorphicNativeWindow >> isMaximized [ 

	^ attributes maximized 
]

{ #category : 'testing' }
MorphicNativeWindow >> isMinimized [

	^ attributes minimized
]

{ #category : 'testing' }
MorphicNativeWindow >> isNotMinimized [

	^ attributes minimized not
]

{ #category : 'testing' }
MorphicNativeWindow >> isNotRestored [
	"Answer whether we are maximised or minimised."

	^attributes  minimized or: [attributes maximized]
]

{ #category : 'testing' }
MorphicNativeWindow >> isOpen [
	
	osWindow ifNil: [ ^ false ].
	^ osWindow isVisible
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> manager [
	^ manager 
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> manager: aWindowManager [ 
	manager := aWindowManager
]

{ #category : 'window management' }
MorphicNativeWindow >> maximize [

	osWindow maximize
]

{ #category : 'window management' }
MorphicNativeWindow >> minimize [

	osWindow minimize
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> open [

	backend := window openInExternalWindow.
	osWindow := self backend worldState worldRenderer osWindow.
	attributes := osWindow initialAttributes.
	
	osWindow announcer weak
		when: DeleteWindowByCrossAnnouncement
		send: #deleteByCross
		to: self.
	window announcer weak
		when: WindowTitleAnnouncement
		send: #windowTitleChanged:
		to: self
]

{ #category : 'accessing' }
MorphicNativeWindow >> osWindow [

^	osWindow
]

{ #category : 'window management' }
MorphicNativeWindow >> restore [

	osWindow restore
]

{ #category : 'taskbar-required' }
MorphicNativeWindow >> taskbarButtonIsActive [

	"true/false"
	
	^ true
]

{ #category : 'taskbar-public' }
MorphicNativeWindow >> taskbarButtonMenu: aMenu [

	| menu theme submenu  |
	theme :=  Smalltalk ui theme.	
	menu := theme newMenuIn: self for: self.
	
	menu
		addToggle: 'Restore' translated
		target: self
		selector: #restore
		getStateSelector: nil
		enablementSelector: #isNotRestored.
	menu lastItem
		iconFormSet: theme windowMaximizeFormSet;
		font: theme menuFont.
		
	menu
		addToggle: 'Minimize' translated
		target: self
		selector: #minimize
		getStateSelector: nil
		enablementSelector: #isNotMinimized.
	menu lastItem
		iconFormSet: theme windowMinimizeFormSet;
		font: theme menuFont.
		
	menu
		addToggle: 'Maximize' translated
		target: self
		selector: #maximize
		getStateSelector: nil
		enablementSelector: #canBeMaximized.
	menu lastItem
		iconFormSet: theme windowMaximizeFormSet;
		font: theme menuFont.
		
	menu addLine.

	submenu := theme newMenuIn: self for: self.
	menu
		add: 'Close all'
		iconFormSet: theme windowCloseFormSet
		subMenu: submenu.
	submenu
		addToggle: 'Close all' translated
		target: self manager
		selector: #closeAllWindows
		getStateSelector: nil
		enablementSelector: true.
	submenu
		addToggle: 'windows to left' translated
		target: self
		selector: #closeAllWindowToLeft
		getStateSelector: nil
		enablementSelector: true.
	submenu
		addToggle: 'windows to right' translated
		target: self
		selector: #closeAllWindowToRight
		getStateSelector: nil
		enablementSelector: true.
	submenu
		addToggle: 'close all debuggers' translated
		target: OupsDebuggerSystem
		selector: #closeAllDebuggers
		getStateSelector: nil
		enablementSelector: true.
	menu
		addToggle: 'Close' translated
		target: self
		selector: #delete
		getStateSelector: nil
		enablementSelector: #allowedToClose.
	menu lastItem
		iconFormSet: theme windowCloseFormSet;
		font: theme menuFont.

	^menu
]

{ #category : 'icons' }
MorphicNativeWindow >> taskbarIcon [

 ^ window class taskbarIcon
]

{ #category : 'taskbar-required' }
MorphicNativeWindow >> taskbarLabel [
	"Answer the label to use for a taskbar button for the receiver."
	
	^ self attributs title
]

{ #category : 'taskbar-required' }
MorphicNativeWindow >> taskbarState [
	"Answer one of #minimized, #restored, #maximized or #active."
	
	^ self attributs minimized
		ifTrue: [#minimized]
		ifFalse: [self attributs maximized
			ifTrue: [#maximized]
			ifFalse: [self attributs  windowCentered
						ifTrue: [#active]
						ifFalse: [#restored]]]
]

{ #category : 'window management' }
MorphicNativeWindow >> theme [
	^ Smalltalk ui theme
]

{ #category : 'getter' }
MorphicNativeWindow >> window [
	^ window
]

{ #category : 'submorphs - add/remove' }
MorphicNativeWindow >> window: aWindow [
	window := aWindow
]

{ #category : 'window management' }
MorphicNativeWindow >> windowTitleChanged: anAnnouncement [

	osWindow title: anAnnouncement title
]
