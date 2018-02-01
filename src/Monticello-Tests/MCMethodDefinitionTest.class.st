Class {
	#name : #MCMethodDefinitionTest,
	#superclass : #MCTestCase,
	#instVars : [
		'navigation',
		'isModified',
		'extensionPackage'
	],
	#category : #'Monticello-Tests-Base'
}

{ #category : #mocks }
MCMethodDefinitionTest >> override [ ^ 1
]

{ #category : #running }
MCMethodDefinitionTest >> ownPackage [
	^ MCWorkingCopy forPackage: (MCPackage named: 'Monticello')
]

{ #category : #running }
MCMethodDefinitionTest >> setUp [
	super setUp.
	extensionPackage := (MCWorkingCopy forPackage: (MCPackage named: 'FooBarBaz')).
	navigation := (Smalltalk hasClassNamed: #SystemNavigation)
		ifTrue: [ (Smalltalk globals at: #SystemNavigation) new ]
		ifFalse: [ Smalltalk ].
	isModified := self ownPackage modified
]

{ #category : #running }
MCMethodDefinitionTest >> tearDown [
      self restoreMocks.
      extensionPackage unregister.
		extensionPackage := nil.
      self class compile: 'override ^ 1' classified: 'mocks'.
      self ownPackage modified: isModified.
		super tearDown
       "FIXME: Unregister Monticellomocks if it got created implicitly.
       This avoids a nasty failure of MCChangeNotificationTest due to
       some inconsistency about whether package names are case sensitive
       or not. They're treated as case insensitive in some name lookups but not
       in others; most importantly PackageOrganizer default treats package
       names as being case sensitive. The package created here is Monticellomocks
       (lower case mocks) and an instance of PackageInfo; the package expected
       in MCChangeNotificationTest is MonticelloMocks and an instance of
       MCMockPackageInfo. Since *that* lookup is case insensitive it can
       find Monticellomocks instead of MonticelloMocks and fail."
       "PackageOrganizer default unregisterPackageNamed: 'MonticelloMocks'."
]

{ #category : #testing }
MCMethodDefinitionTest >> testCannotLoad [
	| definition |
	definition := self mockMethod: #kjahs87 class: 'NoSuchClass' source: 'kjahs87 ^self' meta: false.
	self should: [definition load] raise: Error.
	self assert: (navigation allImplementorsOf: #kjahs87) isEmpty
]

{ #category : #testing }
MCMethodDefinitionTest >> testComparison [
	|d1 d2 d3 d4 d5 |
	d1 := self mockMethod: #one class: 'A' source: '1' meta: false.
	d2 := self mockMethod: #one class: 'A' source: '2' meta: false.
	d3 := self mockMethod: #one class: 'A' source: '1' meta: true.
	d4 := self mockMethod: #two class: 'A' source: '1' meta: false.
	d5 := self mockMethod: #two class: 'A' source: '1' meta: false.
	
	self assert: (d1 isRevisionOf: d2).
	self deny: (d1 isSameRevisionAs: d2).
	
	self deny: (d1 isRevisionOf: d3).
	self deny: (d1 isRevisionOf: d4).
	
	self assert: (d4 isSameRevisionAs: d5).
]

{ #category : #testing }
MCMethodDefinitionTest >> testLoadAndUnload [
	|definition|
	definition := self mockMethod: #one class: 'MCMockClassA' source: 'one ^2' meta: false.
	self assert: self mockInstanceA one = 1.
	definition load.
	self assert: self mockInstanceA one = 2.
	definition unload.
	self deny: (self mockInstanceA respondsTo: #one)
]

{ #category : #testing }
MCMethodDefinitionTest >> testPartiallyRevertOverrideMethod [
	| definition |
	self class compile: 'override ^ 2' classified: '*foobarbaz'.
	self class compile: 'override ^ 3' classified: self mockOverrideMethodCategory.
	self class compile: 'override ^ 4' classified: self mockOverrideMethodCategory.
	definition := (RGMethodDefinition realClass: self class selector: #override) asMCMethodDefinition.
	self assert: definition isOverrideMethod.
	self assert: self override = 4.
	definition unload.
	self assert: self override = 2.
	self assert: (RGMethodDefinition realClass: self class selector: #override) category = '*foobarbaz'.
	
]

{ #category : #testing }
MCMethodDefinitionTest >> testRevertOldMethod [
	| definition changeRecord |
	Object compile: 'yourself ^ self' classified: '*MonticelloMocks'.
	definition := (RGMethodDefinition realClass: Object selector: #yourself) asMCMethodDefinition.
	changeRecord := definition overridenMethodOrNil.
	self assert: changeRecord notNil.
	self assert: changeRecord category = 'accessing'.
	changeRecord fileIn.
]

{ #category : #testing }
MCMethodDefinitionTest >> testRevertOverrideMethod [
	| definition |
	self class compile: 'override ^ 2' classified: self mockOverrideMethodCategory.
	definition := (RGMethodDefinition realClass: self class selector: #override) asMCMethodDefinition.
	self assert: definition isOverrideMethod.
	self assert: self override = 2.
	definition unload.
	self assert: self override = 1.
	self assert: (RGMethodDefinition realClass: self class selector: #override) category = 'mocks'.
	
]
