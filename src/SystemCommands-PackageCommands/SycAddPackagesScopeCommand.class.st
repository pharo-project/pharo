"
I am a command to add packages scope to listScopes
"
Class {
	#name : 'SycAddPackagesScopeCommand',
	#superclass : 'ClyCommand',
	#instVars : [
		'packages'
	],
	#category : 'SystemCommands-PackageCommands-Scope',
	#package : 'SystemCommands-PackageCommands',
	#tag : 'Scope'
}

{ #category : 'execution' }
SycAddPackagesScopeCommand >> execute [

	| scope |
	
	scope := ScopesManager newScopeFrom: packages.
	ScopesManager addScope: scope.
	ScopesPresenter openSelecting: scope
]

{ #category : 'accessing' }
SycAddPackagesScopeCommand >> iconName [
	^ #smallPushpin
]

{ #category : 'accessing' }
SycAddPackagesScopeCommand >> name [
	^ 'Create scope from selected package(s)'
]

{ #category : 'accessing' }
SycAddPackagesScopeCommand >> packages [

	^ packages
]

{ #category : 'accessing' }
SycAddPackagesScopeCommand >> packages: anObject [

	packages := anObject
]

{ #category : 'execution' }
SycAddPackagesScopeCommand >> prepareFullExecutionInContext: aBrowserContext [

	super prepareFullExecutionInContext: aBrowserContext.
	packages := aBrowserContext selectedPackageItems collect: [:e | e actualObject ]
]
