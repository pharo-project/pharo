"
I am a command to add packages scope to listScopes
"
Class {
	#name : 'SycAddPackagesScopeCommand',
	#superclass : 'SycPackageCommand',
	#category : 'SystemCommands-PackageCommands-Scope',
	#package : 'SystemCommands-PackageCommands',
	#tag : 'Scope'
}

{ #category : 'execution' }
SycAddPackagesScopeCommand >> executeRefactoring [

	| scope |
	scope := ScopesManager newScopeFrom: self packages.
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
SycAddPackagesScopeCommand >> order [

	^ 1000
]

{ #category : 'execution' }
SycAddPackagesScopeCommand >> prepareFullExecution [

	packages := self context selectedPackageItems collect: [ :packageItem | packageItem actualObject ]
]
