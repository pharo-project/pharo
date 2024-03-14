"
I am a bridge to allow displaying user defined scopes on Calypso
"
Class {
	#name : 'ClyScopedSystemEnvironment',
	#superclass : 'ClySystemEnvironment',
	#instVars : [
		'systemEnvironment',
		'scope'
	],
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> asGlobalScopeIn: aNavigationEnvironment [
	^ super asGlobalScopeIn: aNavigationEnvironment
 
	"^ClyPackageScope of: Caro package in: aNavigationEnvironment named: name"
]

{ #category : 'converting' }
ClyScopedSystemEnvironment >> asRBEnvironment [
	^ scope
]

{ #category : 'class management' }
ClyScopedSystemEnvironment >> classNamed: aString [
	
	^globals classNamed: aString
]

{ #category : 'class management' }
ClyScopedSystemEnvironment >> classNamed: aString ifAbsent: aBlockClosure [

	^ globals
		  at: aString
		  ifPresent: [ :existing |
			  existing isBehavior
				  ifTrue: [
					  (scope includesClass: existing)
						  ifTrue: [ existing ]
						  ifFalse: aBlockClosure ]
				  ifFalse: aBlockClosure ]
		  ifAbsent: aBlockClosure
]

{ #category : 'class management' }
ClyScopedSystemEnvironment >> classNamed: aString ifPresent: aBlockClosure ifAbsent: anotherBlockClosure [

	^ globals
		  at: aString
		  ifPresent: aBlockClosure
		  ifAbsent: anotherBlockClosure
]

{ #category : 'compiling' }
ClyScopedSystemEnvironment >> compileANewClassFrom: newClassDefinitionString notifying: aController startingFrom: oldClass [
	"The receiver's textual content is a request to define a new class or trait. The
	source code is defString. If any errors occur in compilation, notify
	aController."

	| newClassName newClass newClassPackageName |
	newClassPackageName := self extractPackageNameFrom: newClassDefinitionString.
	(scope definesPackageNamed: newClassPackageName) 
		ifFalse: [ 
			self inform: 'Impossible to create a class in a package outside the scope'. 
			^ nil ].
	
	newClassName := self extractNameFrom: newClassDefinitionString.
	((self isOverridingExistingClassWhenDefiningClassNamed: newClassName startingFrom: oldClass) and: [
		 (self confirmToOverrideExistingClassNamed: newClassName) not ]) ifTrue: [ ^ nil ].

	"On parser & semantic errors (including undeclared because we are in the legacy scripting interactive mode),
	there is UI notification.
	On execution, some methods might be recompiled and cause issues:
	* Warnings (undeclared/shadowed) are sillently passed, but should be fixed by the user later.
	* Errors are unlikely (method should be already broken), but cause (resumable) syntax error for now."
	newClass := (self classCompilerFor: oldClass)
		            source: newClassDefinitionString;
		            requestor: aController;
		            failBlock: [ ^ nil ];
		            logged: true;
		            evaluate.

	newClass := newClass fluidInstall.

	"evaluation can lead to anything (just put a literal in the class defintion pane and accept), these are ignored"
	^ (newClass isBehavior or: [ newClass isTrait ])
		  ifTrue: [ newClass ]
		  ifFalse: [ nil ]
]

{ #category : 'package management' }
ClyScopedSystemEnvironment >> createPackageNamed: packageName [
	"ToDo: Should we allow new packages creation in calypso while displaying a scope?"
	^ packageOrganizer addPackage: packageName
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> ensurePackage: packageName [
	"Should we allow creating new packages while calypso is displaying a scope?".
	
	^ packageOrganizer ensurePackage: packageName
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> extractPackageNameFrom: aDefinitionString [

	^ (CDFluidClassDefinitionParser parse: aDefinitionString) packageName
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> includesClassNamed: aSymbol [
	|class|
	
	class := globals at: aSymbol ifAbsent: [^ false].
	^ scope includesClass: class
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> packageNamed: aString [
	
	^packageOrganizer packageNamed: aString
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> packages [
	
	^ scope packages
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> scope: aRBEnvironment [ 
	scope := aRBEnvironment
]
