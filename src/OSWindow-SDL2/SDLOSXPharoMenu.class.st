"
I am OSX specific operation that creates the menu bar and add the options to it.
I am called by the SDL platform when starting. 
"
Class {
	#name : 'SDLOSXPharoMenu',
	#superclass : 'Object',
	#instVars : [
		'pharoMenu',
		'aboutPharoItem',
		'quitPharoItem',
		'pharoMenuTargetClass',
		'doAboutCallback',
		'pharoTargetObject',
		'doQuitCallback'
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
SDLOSXPharoMenu >> addPharoMenuOptions [

	| selAddMenuItem selSetTarget selDoAbout selDoQuit selRemoveAllItems |
	selAddMenuItem := ObjCLibrary uniqueInstance lookupSelector: 'addItemWithTitle:action:keyEquivalent:'.
	selSetTarget := ObjCLibrary uniqueInstance lookupSelector: 'setTarget:'.
	selDoAbout := ObjCLibrary uniqueInstance lookupSelector: 'doAbout'.
	selDoQuit := ObjCLibrary uniqueInstance lookupSelector: 'doQuit'.
	selRemoveAllItems := ObjCLibrary uniqueInstance lookupSelector: 'removeAllItems'.

	ObjCLibrary uniqueInstance sendMessage: selRemoveAllItems to: self pharoMenu.
	
	aboutPharoItem := ObjCLibrary uniqueInstance
		sendMessage: selAddMenuItem
		to: self pharoMenu
		with: (ObjCLibrary uniqueInstance nsStringOf: 'About Pharo')
		with: selDoAbout
		with: (ObjCLibrary uniqueInstance nsStringOf: '').

	ObjCLibrary uniqueInstance sendMessage: selSetTarget to: aboutPharoItem withPointer: pharoTargetObject.

	quitPharoItem := ObjCLibrary uniqueInstance
		sendMessage: selAddMenuItem
		to: self pharoMenu
		with: (ObjCLibrary uniqueInstance nsStringOf: 'Quit Pharo')
		with: selDoQuit
		with: (ObjCLibrary uniqueInstance nsStringOf: 'q').
		
		ObjCLibrary uniqueInstance sendMessage: selSetTarget to: quitPharoItem withPointer: pharoTargetObject

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
		
	doAboutCallback := FFICallback signature: #(void (void* rcvr, void* objCSel)) block: [:rcvr :sel | self doAbout].	
		
	ObjCLibrary uniqueInstance class_addMethodClass: pharoMenuTargetClass selector: (ObjCLibrary uniqueInstance lookupSelector: 'doAbout') implementation: doAboutCallback  signature: 'v@:'.

	doQuitCallback := FFICallback signature: #(void (void* rcvr, void* objCSel)) block: [:rcvr :sel | self doQuit].	

	ObjCLibrary uniqueInstance class_addMethodClass: pharoMenuTargetClass selector: (ObjCLibrary uniqueInstance lookupSelector: 'doQuit') implementation: doQuitCallback  signature: 'v@:'.
	
	ObjCLibrary uniqueInstance objc_registerClassPair: pharoMenuTargetClass.
	
	pharoTargetObject := ObjCLibrary uniqueInstance sendMessage: selAlloc to: pharoMenuTargetClass.

]

{ #category : 'menu options' }
SDLOSXPharoMenu >> doAbout [

	UIManager default defer: [ Smalltalk aboutThisSystem ]
]

{ #category : 'private' }
SDLOSXPharoMenu >> doCreatePharoMenu [

	| selSharedApplication selItemAtIndex clsNSApplication sharedApplication menu pharoMenuItem selSetMainMenu selSetSubMenu selAlloc selAddMenuItem clsNSMenu |

	selSharedApplication := ObjCLibrary uniqueInstance lookupSelector: 'sharedApplication'.
	selSetMainMenu := ObjCLibrary uniqueInstance lookupSelector: 'setMainMenu:'.
	selItemAtIndex := ObjCLibrary uniqueInstance lookupSelector: 'itemAtIndex:'.
	selSetSubMenu := ObjCLibrary uniqueInstance lookupSelector: 'setSubmenu:'.
	selAlloc := ObjCLibrary uniqueInstance lookupSelector: 'alloc'.
	selAddMenuItem := ObjCLibrary uniqueInstance lookupSelector: 'addItemWithTitle:action:keyEquivalent:'.

	clsNSApplication := ObjCLibrary uniqueInstance lookupClass: 'NSApplication'.
	clsNSMenu := ObjCLibrary uniqueInstance lookupClass: 'NSMenu'.

	menu := ObjCLibrary uniqueInstance sendMessage: selAlloc to: clsNSMenu.
	
	pharoMenuItem := ObjCLibrary uniqueInstance
		sendMessage: selAddMenuItem
		to: menu
		with: (ObjCLibrary uniqueInstance nsStringOf: 'Pharo')
		with: ExternalAddress null
		with: (ObjCLibrary uniqueInstance nsStringOf: '').

	pharoMenu := ObjCLibrary uniqueInstance sendMessage: selAlloc to: clsNSMenu. 
	
	ObjCLibrary uniqueInstance sendMessage: selSetSubMenu to: pharoMenuItem withPointer: pharoMenu.
	
	sharedApplication := ObjCLibrary uniqueInstance sendMessage: selSharedApplication to: clsNSApplication.

	ObjCLibrary uniqueInstance sendMessage: selSetMainMenu to: sharedApplication withPointer: menu.

	^ pharoMenu
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

{ #category : 'accessing' }
SDLOSXPharoMenu >> pharoMenu [
	
	^( pharoMenu isNil or: [ pharoMenu isNull ])
		ifTrue: [ pharoMenu := self doCreatePharoMenu ]
		ifFalse: [ pharoMenu ].
]
