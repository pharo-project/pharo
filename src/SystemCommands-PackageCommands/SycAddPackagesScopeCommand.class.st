"
I am a command to add packages scope to listScopes
"
Class {
	#name : 'SycAddPackagesScopeCommand',
	#superclass : 'CmdCommand',
	#instVars : [
		'packages'
	],
	#category : 'SystemCommands-PackageCommands-Scope',
	#package : 'SystemCommands-PackageCommands',
	#tag : 'Scope'
}

{ #category : 'accessing' }
SycAddPackagesScopeCommand >> defaultMenuIconName [
	^ #add
]

{ #category : 'accessing' }
SycAddPackagesScopeCommand >> defaultMenuItemName [
	^ 'Add package(s) to scope list'
]

{ #category : 'execution' }
SycAddPackagesScopeCommand >> execute [
	| scope |
	scope := ScopesManager newScopeFrom: packages.
	ScopesManager addScope: scope.
	ScopesPresenter open
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
