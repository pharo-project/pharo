"
I am a command to add packages scope to listScopes
"
Class {
	#name : #ClyAddPackagesScopeCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'packages'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #accessing }
ClyAddPackagesScopeCommand >> defaultMenuIconName [ 
	^ #add
]

{ #category : #accessing }
ClyAddPackagesScopeCommand >> defaultMenuItemName [
	^'Add to scope list'
]

{ #category : #execution }
ClyAddPackagesScopeCommand >> execute [
	| scope |

	scope := RBBrowserEnvironment new forPackages: packages.
	RefactoringSettings addScope: scope
]

{ #category : #accessing }
ClyAddPackagesScopeCommand >> packages [

	^ packages
]

{ #category : #accessing }
ClyAddPackagesScopeCommand >> packages: anObject [

	packages := anObject
]

{ #category : #execution }
ClyAddPackagesScopeCommand >> prepareFullExecutionInContext: aBrowserContext [

	super prepareFullExecutionInContext: aBrowserContext.
	packages := aBrowserContext selectedPackageItems collect: [:e | e actualObject ].
]
