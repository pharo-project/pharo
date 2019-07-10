Class {
	#name : #ClySwitchToScopedModeCommand,
	#superclass : #ClyBrowserCommand,
	#instVars : [
		'packages'
	],
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Packages'
}

{ #category : #activation }
ClySwitchToScopedModeCommand class >> fullBrowserMenuActivation [
	<classAnnotation>
	
	^CmdContextMenuActivation byRootGroupItemFor: ClyPackageContextOfFullBrowser 
]

{ #category : #accessing }
ClySwitchToScopedModeCommand >> defaultMenuItemName [
	^'Set up scope'
]

{ #category : #execution }
ClySwitchToScopedModeCommand >> execute [
	
	browser switchToPackageScopeOf: packages
]

{ #category : #accessing }
ClySwitchToScopedModeCommand >> packages [
	^ packages
]

{ #category : #accessing }
ClySwitchToScopedModeCommand >> packages: anObject [
	packages := anObject
]

{ #category : #execution }
ClySwitchToScopedModeCommand >> prepareFullExecutionInContext: aBrowserContext [
	super prepareFullExecutionInContext: aBrowserContext.
	
	packages := browser searchDialog 
		requestMultipleObjects: 'Select package scope' 
		from: ClyAllPackagesQuery sorted.
	packages ifEmpty: [ CmdCommandAborted signal ]
]
