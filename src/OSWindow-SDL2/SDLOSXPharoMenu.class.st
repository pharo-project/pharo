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
	selAddMenuItem := self lookupSelector: 'addItemWithTitle:action:keyEquivalent:'.
	selSetTarget := self lookupSelector: 'setTarget:'.
	selDoAbout := self lookupSelector: 'doAbout'.
	selDoQuit := self lookupSelector: 'doQuit'.
	selRemoveAllItems := self lookupSelector: 'removeAllItems'.

	self sendMessage: selRemoveAllItems to: self pharoMenu.
	
	aboutPharoItem := self
		sendMessage: selAddMenuItem
		to: self pharoMenu
		with: (self nsStringOf: 'About Pharo')
		with: selDoAbout
		with: (self nsStringOf: '').

	self sendMessage: selSetTarget to: aboutPharoItem withPointer: pharoTargetObject.

	quitPharoItem := self
		sendMessage: selAddMenuItem
		to: self pharoMenu
		with: (self nsStringOf: 'Quit Pharo')
		with: selDoQuit
		with: (self nsStringOf: 'q').
		
		self sendMessage: selSetTarget to: quitPharoItem withPointer: pharoTargetObject

]

{ #category : 'private' }
SDLOSXPharoMenu >> allocatePharoMenuTarget [
	
	| nsObjectCls selAlloc |

	(pharoTargetObject isNotNil and: [ pharoTargetObject isNull not ])
		ifTrue: [ ^ self ].

	nsObjectCls := self lookupClass: 'NSObject'.
	selAlloc := self lookupSelector: 'alloc'.

	pharoMenuTargetClass := self 
		objc_allocateClassPairSuperclass: nsObjectCls 
		name: 'PharoMenuTargetClass' 
		extraBytes: 0.
		
	doAboutCallback := FFICallback signature: #(void (void* rcvr, void* objCSel)) block: [:rcvr :sel | self doAbout].	
		
	self class_addMethodClass: pharoMenuTargetClass selector: (self lookupSelector: 'doAbout') implementation: doAboutCallback  signature: 'v@:'.

	doQuitCallback := FFICallback signature: #(void (void* rcvr, void* objCSel)) block: [:rcvr :sel | self doQuit].	

	self class_addMethodClass: pharoMenuTargetClass selector: (self lookupSelector: 'doQuit') implementation: doQuitCallback  signature: 'v@:'.
	
	self objc_registerClassPair: pharoMenuTargetClass.
	
	pharoTargetObject := self sendMessage: selAlloc to: pharoMenuTargetClass.

]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> class_addMethodClass: cls selector: name implementation: imp signature: types [
	^ self ffiCall: #(int class_addMethod(void* cls, void* name, void* imp, const char *types))
]

{ #category : 'menu options' }
SDLOSXPharoMenu >> doAbout [

	UIManager default defer: [ Smalltalk aboutThisSystem ]
]

{ #category : 'private' }
SDLOSXPharoMenu >> doGetPharoMenu [

	| selSharedApplication selMainMenu selNumberOfItems selItemAtIndex selSubMenu clsNSApplication sharedApplication menu pharoMenuItem |

	selSharedApplication := self lookupSelector: 'sharedApplication'.
	selMainMenu := self lookupSelector: 'mainMenu'.
	selNumberOfItems := self lookupSelector: 'numberOfItems'.
	selItemAtIndex := self lookupSelector: 'itemAtIndex:'.
	selSubMenu := self lookupSelector: 'submenu'.

	clsNSApplication := self lookupClass: 'NSApplication'.
	sharedApplication := self sendMessage: selSharedApplication to: clsNSApplication.

	menu := self sendMessage: selMainMenu to: sharedApplication.

	pharoMenuItem := self
		                 sendMessage: selItemAtIndex
		                 to: menu
		                 withInteger: 0.

	^ self sendMessage: selSubMenu to: pharoMenuItem
]

{ #category : 'menu options' }
SDLOSXPharoMenu >> doQuit [

	UIManager default defer: [WorldState quitSession] 
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> ffiLibraryName [

	^ 'libobjc.dylib'
]

{ #category : 'api' }
SDLOSXPharoMenu >> installInOSXWindow [

	self allocatePharoMenuTarget.
	self addPharoMenuOptions.
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> lookupClass: aString [

	^ self ffiCall: #(void* objc_lookUpClass(char *aString))
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> lookupSelector: aString [

	^ self ffiCall: #(void* sel_registerName(const char *aString))
]

{ #category : 'private' }
SDLOSXPharoMenu >> nsStringOf: aString [

	| class selector encoded param |
	class := self lookupClass: 'NSString'.
	selector:= self lookupSelector: 'stringWithUTF8String:'.

	encoded := aString utf8Encoded.
	param := ByteArray new: encoded size + 1.
	param pinInMemory.

	LibC memCopy: encoded to: param size: encoded size.
	param at: encoded size + 1 put: 0.

	^ self sendMessage: selector to: class withPointer: param
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> objc_allocateClassPairSuperclass: superclass name: name extraBytes: extraBytes [
	
	^ self ffiCall: #(void* objc_allocateClassPair(void* superclass, const char *name, size_t extraBytes))
	
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> objc_registerClassPair: cls [
	
	self ffiCall: #(void objc_registerClassPair(void* cls))
	
	
]

{ #category : 'accessing' }
SDLOSXPharoMenu >> pharoMenu [
	
	^( pharoMenu isNil or: [ pharoMenu isNull ])
		ifTrue: [ pharoMenu := self doGetPharoMenu ]
		ifFalse: [ pharoMenu ].
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> sendMessage: sel to: cls [

	^ self ffiCall: #(void* objc_msgSend(void* cls, void* sel))
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> sendMessage: sel to: rcv with: aParam1 with:aParam2 with:aParam3 [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, void* aParam1, void* aParam2, void* aParam3))
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> sendMessage: sel to: rcv withInteger: aParam [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, int aParam))
]

{ #category : 'private - ffi' }
SDLOSXPharoMenu >> sendMessage: sel to: rcv withPointer: aParam [

	^ self ffiCall: #(void* objc_msgSend(void* rcv, void* sel, void* aParam))
]
