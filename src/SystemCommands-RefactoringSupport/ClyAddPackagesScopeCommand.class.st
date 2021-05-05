Class {
	#name : #ClyAddPackagesScopeCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'packages',
		'key'
	],
	#category : #'SystemCommands-RefactoringSupport-Scope'
}

{ #category : #activation }
ClyAddPackagesScopeCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyFullBrowserPackageContext 
]

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

	scope := RBPackageEnvironment	onEnvironment: browser navigationEnvironment packages: packages.
	RefactoringSettings addScope: scope withKey: key.
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
	key := UIManager default request: 'Keyword of scoper'.
	(key isEmptyOrNil or: [ RefactoringSettings invalidNames includes: key ]) ifTrue: [ CmdCommandAborted signal ].
]
