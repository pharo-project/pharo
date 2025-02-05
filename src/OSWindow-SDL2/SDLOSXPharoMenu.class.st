"
I am OSX specific operation that creates the menu bar and add the options to it.
I am called by the SDL platform when starting. 
"
Class {
	#name : 'SDLOSXPharoMenu',
	#superclass : 'Object',
	#instVars : [
		'pharoMenu',
		'pharoMenuTargetClass',
		'doAboutCallback',
		'pharoTargetObject',
		'doQuitCallback',
		'doPreferencesCallback'
	],
	#classVars : [
		'UniqueInstance'
	],
	#category : 'OSWindow-SDL2-Pharo-Specific',
	#package : 'OSWindow-SDL2',
	#tag : 'Pharo-Specific'
}

{ #category : 'instance creation' }
SDLOSXPharoMenu class >> uniqueInstance [

	^ UniqueInstance ifNil: [ UniqueInstance := self new ].
]

{ #category : 'private' }
SDLOSXPharoMenu >> addCallbackForSelector: aSelector withBlock: aBlock toTargetClass: aTargetClass [

	| aCallback |

	aCallback := FFICallback signature: #(void (void* rcvr, void* objCSel)) block: [:rcvr :sel | aBlock value ].			
	ObjCLibrary uniqueInstance class_addMethodClass: aTargetClass selector: (ObjCLibrary uniqueInstance lookupSelector: aSelector) implementation: aCallback  signature: 'v@:'.

	^ aCallback
]

{ #category : 'private' }
SDLOSXPharoMenu >> addMenuItemTo: aNSMenu title: aTitle shortcut: aShortcut selector: aNSSelector [

	| selAddMenuItem selSetTarget newItem selectorPointer |
	selAddMenuItem := ObjCLibrary uniqueInstance lookupSelector: 'addItemWithTitle:action:keyEquivalent:'.
	selSetTarget := ObjCLibrary uniqueInstance lookupSelector: 'setTarget:'.
	
	selectorPointer := aNSSelector ifNil: [ ExternalAddress null ] ifNotNil: [ aNSSelector ].
	
	newItem := ObjCLibrary uniqueInstance
		sendMessage: selAddMenuItem
		to: aNSMenu
		with: (ObjCLibrary uniqueInstance nsStringOf: aTitle)
		with: selectorPointer
		with: (ObjCLibrary uniqueInstance nsStringOf: aShortcut).

	selectorPointer isNull 
		ifFalse: [ ObjCLibrary uniqueInstance sendMessage: selSetTarget to: newItem withPointer: pharoTargetObject].

	^ newItem

]

{ #category : 'private' }
SDLOSXPharoMenu >> addMenuItemTo: menu title: aTitle withSubMenu: aNSMenu [

	| selSetSubMenu menuItem |
	
	selSetSubMenu := ObjCLibrary uniqueInstance lookupSelector: 'setSubmenu:'.
	menuItem := self addMenuItemTo: menu title: aTitle shortcut: '' selector: nil.
	
	ObjCLibrary uniqueInstance sendMessage: selSetSubMenu to: menuItem withPointer: aNSMenu.

]

{ #category : 'private' }
SDLOSXPharoMenu >> addPharoMenuOptions [

	| selDoAbout selDoQuit selRemoveAllItems selDoPreferences |
	selDoAbout := ObjCLibrary uniqueInstance lookupSelector: 'doAbout'.
	selDoQuit := ObjCLibrary uniqueInstance lookupSelector: 'doQuit'.
	selDoPreferences := ObjCLibrary uniqueInstance lookupSelector: 'doPreferences'.

	selRemoveAllItems := ObjCLibrary uniqueInstance lookupSelector: 'removeAllItems'.
	ObjCLibrary uniqueInstance sendMessage: selRemoveAllItems to: self pharoMenu.

	self addMenuItemTo: self pharoMenu title: 'About Pharo' shortcut: '' selector: selDoAbout.
	self addSeparatorTo: self pharoMenu.

	self addMenuItemTo: self pharoMenu title: 'Preferences' shortcut: ',' selector: selDoPreferences.
	self addSeparatorTo: self pharoMenu.
	
	self addServicesMenuTo: self pharoMenu.
	
	self addMenuItemTo: self pharoMenu title: 'Quit Pharo' shortcut: 'q' selector: selDoQuit.
]

{ #category : 'private' }
SDLOSXPharoMenu >> addSeparatorTo: aNSMenu [

	| menuItemCls selSeparatorItem selAddItem newSeparator |
	menuItemCls := ObjCLibrary uniqueInstance lookupClass: 'NSMenuItem'.
	selSeparatorItem := ObjCLibrary uniqueInstance lookupSelector: 'separatorItem'.
	selAddItem := ObjCLibrary uniqueInstance lookupSelector: 'addItem:'.

	newSeparator := ObjCLibrary uniqueInstance sendMessage: selSeparatorItem to: menuItemCls.

	ObjCLibrary uniqueInstance sendMessage: selAddItem to: aNSMenu withPointer: newSeparator.

]

{ #category : 'private' }
SDLOSXPharoMenu >> addServicesMenuTo: aNSMenu [

	| selServicesMenu servicesMenu sharedApplication selSetServicesMenu |
	selServicesMenu := ObjCLibrary uniqueInstance lookupSelector: 'servicesMenu'.
	sharedApplication := ObjCLibrary uniqueInstance sharedApplication.
	servicesMenu := ObjCLibrary uniqueInstance sendMessage: selServicesMenu to: sharedApplication.
	selSetServicesMenu := ObjCLibrary uniqueInstance lookupSelector: 'setServicesMenu:'.

	"If there is no Services Menu, we have to create it and register it"
	servicesMenu isNull ifTrue: [ 
		servicesMenu := self newNSMenu.
		ObjCLibrary uniqueInstance sendMessage: selSetServicesMenu to: sharedApplication  withPointer: servicesMenu].

	self addMenuItemTo: aNSMenu title: 'Services' withSubMenu: servicesMenu.
	self addSeparatorTo: aNSMenu

]

{ #category : 'private' }
SDLOSXPharoMenu >> allocatePharoMenuTarget [
	
	| nsObjectCls selAlloc |

	(pharoTargetObject isNotNil and: [ pharoTargetObject isNull not ])
		ifTrue: [ ^ self ].

	nsObjectCls := ObjCLibrary uniqueInstance lookupClass: 'NSObject'.
	selAlloc := ObjCLibrary uniqueInstance lookupSelector: 'alloc'.

	pharoMenuTargetClass := ObjCLibrary uniqueInstance 
		objc_allocateClassPairSuperclass: nsObjectCls 
		name: 'PharoMenuTargetClass' 
		extraBytes: 0.
	
	doAboutCallback := self addCallbackForSelector: 'doAbout' withBlock: [ self doAbout ] toTargetClass: pharoMenuTargetClass.
	doPreferencesCallback := self addCallbackForSelector: 'doPreferences' withBlock: [ self doPreferences ] toTargetClass: pharoMenuTargetClass.
	doQuitCallback := self addCallbackForSelector: 'doQuit' withBlock: [ self doQuit ] toTargetClass: pharoMenuTargetClass.
			
	ObjCLibrary uniqueInstance objc_registerClassPair: pharoMenuTargetClass.
	
	pharoTargetObject := ObjCLibrary uniqueInstance sendMessage: selAlloc to: pharoMenuTargetClass.

]

{ #category : 'menu options' }
SDLOSXPharoMenu >> doAbout [

	UIManager default defer: [ StPharoSettings openPharoAbout ]
]

{ #category : 'private' }
SDLOSXPharoMenu >> doCreatePharoMenu [

	| selItemAtIndex mainMenu pharoMenuItem selSetMainMenu sharedApplication |

	selSetMainMenu := ObjCLibrary uniqueInstance lookupSelector: 'setMainMenu:'.
	selItemAtIndex := ObjCLibrary uniqueInstance lookupSelector: 'itemAtIndex:'.

	mainMenu := self newNSMenu.		
	pharoMenu := self newNSMenu. 
	
	pharoMenuItem := self addMenuItemTo: mainMenu title: 'Pharo' withSubMenu: pharoMenu.
	
	sharedApplication := ObjCLibrary uniqueInstance sharedApplication.
	ObjCLibrary uniqueInstance sendMessage: selSetMainMenu to: sharedApplication withPointer: mainMenu.

	^ pharoMenu
]

{ #category : 'menu options' }
SDLOSXPharoMenu >> doPreferences [

	UIManager default defer: [ SettingBrowser open ]
]

{ #category : 'menu options' }
SDLOSXPharoMenu >> doQuit [

	UIManager default defer: [WorldState quitSession] 
]

{ #category : 'api' }
SDLOSXPharoMenu >> installInOSXWindow [

	self allocatePharoMenuTarget.
	self addPharoMenuOptions.
]

{ #category : 'private' }
SDLOSXPharoMenu >> newNSMenu [	

	| selAlloc clsNSMenu |
	selAlloc := ObjCLibrary uniqueInstance lookupSelector: 'alloc'.
	clsNSMenu := ObjCLibrary uniqueInstance lookupClass: 'NSMenu'.

	^ ObjCLibrary uniqueInstance sendMessage: selAlloc to: clsNSMenu.

]

{ #category : 'accessing' }
SDLOSXPharoMenu >> pharoMenu [
	
	^( pharoMenu isNil or: [ pharoMenu isNull ])
		ifTrue: [ pharoMenu := self doCreatePharoMenu ]
		ifFalse: [ pharoMenu ].
]
