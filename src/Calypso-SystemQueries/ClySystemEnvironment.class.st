"
I represent environment of Pharo system. I incapsulate globals (Smalltalk globals), packageOrganizer (RPackageOrganizer  default) and changesAnnouncer (SystemAnnouncer uniqueInstance). I have class side #currentImage instance created with all corresponding globals of current image.

I am used to navigate over system by ClyNavigationEnvironment.

Public API and Key Messages

- packages
- createPackageNamed: aString
- removePackage: aPackage
- includesClassNamed: aString 
- defaultClassCompiler
- subscribe: aNavigationEnvironment
- unsubscribe: aNavigationEnvironment

Internal Representation and Key Implementation Points.

    Instance Variables
	changesAnnouncer:		<SystemAnnouncer>
	globals:		<SmalltalkDictionary> ""Smalltalk globals class""
	name:		<String>
	packageOrganizer:		<RPackageOrganizer>
	projectManager:		<ClyProjectManager>
"
Class {
	#name : #ClySystemEnvironment,
	#superclass : #Object,
	#instVars : [
		'name',
		'globals',
		'changesAnnouncer',
		'packageOrganizer',
		'projectManager'
	],
	#classInstVars : [
		'currentImage'
	],
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #accessing }
ClySystemEnvironment class >> currentImage [
	^currentImage ifNil: [
		currentImage := self new
			name: 'current image';
			globals: Smalltalk globals;
			packageOrganizer: RPackageOrganizer default;
			changesAnnouncer: SystemAnnouncer uniqueInstance]
]

{ #category : #initialization }
ClySystemEnvironment class >> reset [
	<script>
	currentImage := nil
]

{ #category : #accessing }
ClySystemEnvironment >> allProcesses [
	^Process allSubInstances 
]

{ #category : #accessing }
ClySystemEnvironment >> asGlobalScopeIn: aNavigationEnvironment [

	
	^ClySystemEnvironmentScope of: self in: aNavigationEnvironment named: name
]

{ #category : #'class management' }
ClySystemEnvironment >> bindingOf: aSymbol [
	^globals bindingOf: aSymbol
]

{ #category : #accessing }
ClySystemEnvironment >> changesAnnouncer [
	^ changesAnnouncer
]

{ #category : #accessing }
ClySystemEnvironment >> changesAnnouncer: anObject [
	changesAnnouncer := anObject
]

{ #category : #'class management' }
ClySystemEnvironment >> classNamed: aString [
	^globals classNamed: aString
]

{ #category : #'class management' }
ClySystemEnvironment >> classNamed: aString ifAbsent: aBlockClosure [ 
	^globals 
		at: aString
		ifPresent: [ :existing | 
			existing isBehavior ifTrue: [ existing ] ifFalse: aBlockClosure] 
		ifAbsent: aBlockClosure
]

{ #category : #'class management' }
ClySystemEnvironment >> classNamed: aString ifPresent: aBlockClosure ifAbsent: anotherBlockClosure [
	^ globals at: aString ifPresent: aBlockClosure ifAbsent: anotherBlockClosure
]

{ #category : #accessing }
ClySystemEnvironment >> classes [
	^ globals allClassesAndTraits
]

{ #category : #'class compilation' }
ClySystemEnvironment >> compileANewClassFrom: aString notifying: aController startingFrom: aClass [
	" Copied from Browser "

	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aController."

	| oldClass class newClassName defTokens keywdIx classCompiler |
	oldClass := aClass.
	defTokens := aString findTokens: Character separators.
	((defTokens first = 'Trait' and: [ defTokens second = 'named:' ])
		or: [ defTokens second = 'classTrait' ])
		ifTrue: [ ^ self
				defineTrait: aString
				notifying: aController
				startingFrom: aClass ].
	keywdIx := defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx := defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName := (defTokens at: keywdIx + 1) copyWithoutAll: '#()'.
	((oldClass isNil
		or: [ oldClass instanceSide name asString ~= newClassName ])
		and: [ self includesClassNamed: newClassName asSymbol ])
		ifTrue: [ "Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self
				confirm:
					((newClassName
						,
							' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText
						makeBoldFrom: 1
						to: newClassName size))
				ifFalse: [ ^ nil ] ].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass
		ifNil: [ classCompiler := self defaultClassCompiler ]
		ifNotNil: [ classCompiler := oldClass superclass subclassDefinerClass new ].
	class := classCompiler
		source: aString;
		requestor: aController;
		logged: true;
		evaluate.
	^ class isBehavior
		ifTrue: [ class ]
		ifFalse: [ nil ]
]

{ #category : #'package management' }
ClySystemEnvironment >> createPackageNamed: packageName [
	^packageOrganizer createPackageNamed: packageName
]

{ #category : #'class compilation' }
ClySystemEnvironment >> defaultClassCompiler [
	^self class compiler
]

{ #category : #'package management' }
ClySystemEnvironment >> ensureExistAndRegisterPackageNamed: packageName [
	^packageOrganizer ensureExistAndRegisterPackageNamed: packageName
]

{ #category : #accessing }
ClySystemEnvironment >> globals [
	^ globals
]

{ #category : #accessing }
ClySystemEnvironment >> globals: anObject [
	globals := anObject
]

{ #category : #'class management' }
ClySystemEnvironment >> includesClassNamed: aSymbol [
	^globals includesKey: aSymbol
]

{ #category : #initialization }
ClySystemEnvironment >> initialize [
	super initialize.
	projectManager := ClyProjectManagerRegistry new
]

{ #category : #accessing }
ClySystemEnvironment >> name [
	^ name ifNil: [ super printString ]
]

{ #category : #accessing }
ClySystemEnvironment >> name: anObject [
	name := anObject
]

{ #category : #'package management' }
ClySystemEnvironment >> packageNamed: aString [ 
	^packageOrganizer packageNamed: aString
]

{ #category : #accessing }
ClySystemEnvironment >> packageOrganizer [
	^ packageOrganizer
]

{ #category : #accessing }
ClySystemEnvironment >> packageOrganizer: anObject [
	packageOrganizer := anObject
]

{ #category : #accessing }
ClySystemEnvironment >> packages [
	^ packageOrganizer packages
]

{ #category : #printing }
ClySystemEnvironment >> printOn: aStream [
	super printOn: aStream.
	name ifNil: [ ^self ].
	
	aStream nextPut: $(.
	aStream nextPutAll: name.
	aStream nextPut: $).
]

{ #category : #accessing }
ClySystemEnvironment >> projectManager [
	^ projectManager
]

{ #category : #accessing }
ClySystemEnvironment >> projectManager: anObject [
	projectManager := anObject
]

{ #category : #accessing }
ClySystemEnvironment >> projects [
	^projectManager projects
]

{ #category : #'package management' }
ClySystemEnvironment >> registerProjectManager: aPackageManager [
	aPackageManager systemEnvironment: self.
	projectManager registerProjectManager: aPackageManager
]

{ #category : #'package management' }
ClySystemEnvironment >> removePackage: aPackage [

	packageOrganizer unregisterPackage: aPackage
]

{ #category : #subscription }
ClySystemEnvironment >> subscribe: anObject [
	self subscribe: anObject for: SystemAnnouncement
]

{ #category : #subscription }
ClySystemEnvironment >> subscribe: anObject for: anAnnouncementClass [
	self when: anAnnouncementClass send: #systemChanged: to: anObject 
]

{ #category : #subscription }
ClySystemEnvironment >> unsubscribe: anObject [
	changesAnnouncer unsubscribe: anObject
]

{ #category : #subscription }
ClySystemEnvironment >> when: anAnnouncementClass send: aSelector to: anObject [
	changesAnnouncer weak when: anAnnouncementClass send: aSelector to: anObject
]
