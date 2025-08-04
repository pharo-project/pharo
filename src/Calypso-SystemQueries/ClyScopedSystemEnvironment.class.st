"
I am a bridge to allow displaying user defined scopes on Calypso
"
Class {
	#name : 'ClyScopedSystemEnvironment',
	#superclass : 'ClySystemEnvironment',
	#instVars : [
		'scope'
	],
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'comparing' }
ClyScopedSystemEnvironment >> = anObject [

	^ anObject class = self class and: [ anObject scope = self scope ]
]

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

{ #category : 'compiling' }
ClyScopedSystemEnvironment >> compileANewClassFrom: newClassDefinitionString notifying: aController startingFrom: oldClass [
	"The receiver's textual content is a request to define a new class or trait. The
	source code is defString. If any errors occur in compilation, notify
	aController."

	| newClassPackageName |
	newClassPackageName := self extractPackageNameFrom:
		                       newClassDefinitionString.
	(scope definesPackageNamed: newClassPackageName) ifFalse: [
		self inform:
			'Impossible to create a class in a package outside the scope'.
		^ nil ].

	^ super compileANewClassFrom: newClassDefinitionString notifying: aController startingFrom: oldClass
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> definedClassesInPackage: aPackage [

	^ scope definedClasses select: [ :class | class package = aPackage ]
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> extractPackageNameFrom: aDefinitionString [

	^ (CDFluidClassDefinitionParser parse: aDefinitionString) packageName
]

{ #category : 'comparing' }
ClyScopedSystemEnvironment >> hash [

	^ self scope hash
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> includesClassNamed: aSymbol [
	|class|
	
	class := globals at: aSymbol ifAbsent: [^ false].
	^ scope includesClass: class
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> packages [
	
	^ scope packages
]

{ #category : 'printing' }
ClyScopedSystemEnvironment >> printOn: aStream [

	| title |
	title := self class name.
	aStream
		nextPutAll: (title first isVowel
				 ifTrue: [ 'an ' ]
				 ifFalse: [ 'a ' ]);
		nextPutAll: title.

	aStream nextPut: $(.
	aStream nextPutAll: scope label.
	aStream nextPut: $)
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> scope [
	^ scope
]

{ #category : 'accessing' }
ClyScopedSystemEnvironment >> scope: aRBEnvironment [ 
	scope := aRBEnvironment
]
