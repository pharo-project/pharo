"
I represent environment of Pharo system. I incapsulate globals (Smalltalk globals). I have class side #currentImage instance created with all corresponding globals of current image.

I am used to navigate over system by ClyNavigationEnvironment.

Public API and Key Messages

- packages
- createPackageNamed: aString
- includesClassNamed: aString 
- defaultClassCompiler
- subscribe: aNavigationEnvironment
- unsubscribe: aNavigationEnvironment

Internal Representation and Key Implementation Points.

    Instance Variables
	globals:		<SmalltalkDictionary> ""Smalltalk globals class""
	name:		<String>
	projectManager:		<ProjectManager>
"
Class {
	#name : 'ClySystemEnvironment',
	#superclass : 'Object',
	#instVars : [
		'name',
		'globals',
		'projectManager'
	],
	#classInstVars : [
		'currentImage'
	],
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'accessing' }
ClySystemEnvironment class >> currentImage [

	^ currentImage ifNil: [
		  currentImage := self new
			                  name: 'Current image';
			                  globals: Smalltalk globals ]
]

{ #category : 'class initialization' }
ClySystemEnvironment class >> reset [
	<script>
	currentImage := nil
]

{ #category : 'accessing' }
ClySystemEnvironment >> allProcesses [
	^Process allSubInstances
]

{ #category : 'accessing' }
ClySystemEnvironment >> asGlobalScopeIn: aNavigationEnvironment [


	^ClySystemEnvironmentScope of: self in: aNavigationEnvironment named: name
]

{ #category : 'converting' }
ClySystemEnvironment >> asRBEnvironment [
	^ self environment asRBEnvironment
]

{ #category : 'class management' }
ClySystemEnvironment >> bindingOf: aSymbol [
	^globals bindingOf: aSymbol
]

{ #category : 'accessing' }
ClySystemEnvironment >> changesAnnouncer [

	^ self globals codeChangeAnnouncer
]

{ #category : 'class compilation' }
ClySystemEnvironment >> classCompilerFor: aClass [

	"Use aClass superclass because it knows the definerClass of aClass."

	^ aClass
		  ifNil: [ self defaultClassCompiler ]
		  ifNotNil: [ aClass superclass subclassDefinerClass new ]
]

{ #category : 'class management' }
ClySystemEnvironment >> classNamed: aString [
	^globals classNamed: aString
]

{ #category : 'class management' }
ClySystemEnvironment >> classNamed: aString ifAbsent: aBlockClosure [
	^globals
		at: aString
		ifPresent: [ :existing |
			existing isBehavior ifTrue: [ existing ] ifFalse: aBlockClosure]
		ifAbsent: aBlockClosure
]

{ #category : 'class management' }
ClySystemEnvironment >> classNamed: aString ifPresent: aBlockClosure ifAbsent: anotherBlockClosure [
	^ globals at: aString ifPresent: aBlockClosure ifAbsent: anotherBlockClosure
]

{ #category : 'accessing' }
ClySystemEnvironment >> classes [
	^ globals allClassesAndTraits
]

{ #category : 'compiling' }
ClySystemEnvironment >> compileANewClassFrom: newClassDefinitionString notifying: aController startingFrom: oldClass [
	"The receiver's textual content is a request to define a new class or trait. The
	source code is defString. If any errors occur in compilation, notify
	aController."

	| newClassName newClass compiler |
	newClassName := self extractNameFrom: newClassDefinitionString.
	((self
		  isOverridingExistingClassWhenDefiningClassNamed: newClassName
		  startingFrom: oldClass) and: [
		 (self confirmToOverrideExistingClassNamed: newClassName) not ])
		ifTrue: [ ^ nil ].

	"On parser & semantic errors (including undeclared because we are in the legacy scripting interactive mode),
	there is UI notification.
	On execution, some methods might be recompiled and cause issues:
	* Warnings (undeclared/shadowed) are sillently passed, but should be fixed by the user later.
	* Errors are unlikely (method should be already broken), but cause (resumable) syntax error for now."

	compiler := (self classCompilerFor: oldClass)
		            source: newClassDefinitionString;
		            requestor: aController;
		            failBlock: [ ^ nil ];
		            logged: true.
	
	[newClass := compiler evaluate] 
		on: Error do: [:e | self notifyErroInClassDefinition: e. ^ nil].

	newClass := newClass fluidInstall.

	"evaluation can lead to anything (just put a literal in the class defintion pane and accept), these are ignored"
	^ (newClass isBehavior or: [ newClass isTrait ])
		  ifTrue: [ newClass ]
		  ifFalse: [ nil ]
]

{ #category : 'class management' }
ClySystemEnvironment >> confirmToOverrideExistingClassNamed: newClassName [
	"Attempting to define new class/trait over existing one when not looking at the original one in this browser..."

	^ self confirm: ((newClassName , ' might have been edited from another editor.
Redefining it might override these changes.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newClassName size)
]

{ #category : 'package management' }
ClySystemEnvironment >> createPackageNamed: packageName [

	^ self packageOrganizer addPackage: packageName
]

{ #category : 'class compilation' }
ClySystemEnvironment >> defaultClassCompiler [

	^ self class compiler
]

{ #category : 'accessing' }
ClySystemEnvironment >> definedClassesInPackage: aPackage [

	^ aPackage definedClasses
]

{ #category : 'package management' }
ClySystemEnvironment >> ensurePackage: packageName [

	^ self packageOrganizer ensurePackage: packageName
]

{ #category : 'accessing' }
ClySystemEnvironment >> environment [
	^ RBBrowserEnvironment new
]

{ #category : 'class management' }
ClySystemEnvironment >> extractNameFrom: aDefinitionString [

	^ (aDefinitionString findTokens: Character separators , '<#') at: 2 ifAbsent: [ '' ]
]

{ #category : 'accessing' }
ClySystemEnvironment >> globals [
	^ globals
]

{ #category : 'accessing' }
ClySystemEnvironment >> globals: anObject [
	globals := anObject
]

{ #category : 'class management' }
ClySystemEnvironment >> includesClassNamed: aSymbol [
	^globals includesKey: aSymbol
]

{ #category : 'initialization' }
ClySystemEnvironment >> initialize [
	super initialize.
	projectManager := ProjectManager new
]

{ #category : 'class management' }
ClySystemEnvironment >> isOverridingExistingClassWhenDefiningClassNamed: newClassName startingFrom: oldClass [
	"Attempting to define new class over existing one when not looking at the original one in this browser..."

	^ (oldClass isNil or: [ oldClass instanceSide name asString ~= newClassName ]) and: [ self includesClassNamed: newClassName asSymbol ]
]

{ #category : 'accessing' }
ClySystemEnvironment >> name [
	^ name ifNil: [ super printString ]
]

{ #category : 'accessing' }
ClySystemEnvironment >> name: anObject [
	name := anObject
]

{ #category : 'ui' }
ClySystemEnvironment >> notifyErroInClassDefinition: anError [ 

	| text |

	anError freeze.

	text := ((anError isKindOf: MessageNotUnderstood) and: [ anError receiver isKindOf: ShiftClassBuilder ])
		ifTrue: [ 'The selector {1} is unrecognized in your class definition.'
		        format: { anError message selector } ]
		ifFalse: [ anError messageText ].

	SpConfirmDialog new
		title: 'There is an error in your class definition';
		label: text;
		acceptLabel: 'Debug';
		cancelLabel: 'Close';
		onAccept: [ anError debug ];
		openDialog
		
]

{ #category : 'package management' }
ClySystemEnvironment >> packageNamed: aString [

	^ self packageOrganizer packageNamed: aString
]

{ #category : 'accessing' }
ClySystemEnvironment >> packageOrganizer [

	^ self globals packageOrganizer
]

{ #category : 'accessing' }
ClySystemEnvironment >> packages [

	^ self packageOrganizer packages
]

{ #category : 'printing' }
ClySystemEnvironment >> printOn: aStream [
	super printOn: aStream.
	name ifNil: [ ^self ].

	aStream nextPut: $(.
	aStream nextPutAll: name.
	aStream nextPut: $)
]

{ #category : 'accessing' }
ClySystemEnvironment >> projectManager [
	^ projectManager
]

{ #category : 'accessing' }
ClySystemEnvironment >> projectManager: anObject [
	projectManager := anObject
]

{ #category : 'accessing' }
ClySystemEnvironment >> projects [
	^projectManager projects
]

{ #category : 'subscription' }
ClySystemEnvironment >> subscribe: anObject [
	self subscribe: anObject for: SystemAnnouncement
]

{ #category : 'subscription' }
ClySystemEnvironment >> subscribe: anObject for: anAnnouncementClass [
	self when: anAnnouncementClass send: #systemChanged: to: anObject
]

{ #category : 'subscription' }
ClySystemEnvironment >> unsubscribe: anObject [

	self changesAnnouncer unsubscribe: anObject
]

{ #category : 'subscription' }
ClySystemEnvironment >> when: anAnnouncementClass send: aSelector to: anObject [

	self changesAnnouncer weak when: anAnnouncementClass send: aSelector to: anObject
]
