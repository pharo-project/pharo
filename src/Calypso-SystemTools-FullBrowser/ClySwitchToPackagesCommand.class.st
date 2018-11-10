"
I am a command to switch full browser to raw packages mode. 
"
Class {
	#name : #ClySwitchToPackagesCommand,
	#superclass : #ClySwitchPackageViewModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Packages'
}

{ #category : #activation }
ClySwitchToPackagesCommand class >> toolbarOrder [
	^1
]

{ #category : #accessing }
ClySwitchToPackagesCommand >> defaultMenuItemName [
	^browser isScopedModeEnabled 
		ifTrue: [ 'Scoped pckg' ]
		ifFalse: [ 'Packages' ]
]

{ #category : #accessing }
ClySwitchToPackagesCommand >> description [
	self isAppliedToBrowser ifFalse: [ ^'Click to show packages' ].
	
	^browser isScopedModeEnabled 
		ifTrue: [ 'Click to reset scope' ]
		ifFalse: [ 'Click to scope by selected packages' ]
]

{ #category : #execution }
ClySwitchToPackagesCommand >> execute [
	self isAppliedToBrowser ifFalse: [ ^browser switchToPackages].
	browser isScopedModeEnabled ifTrue: [ ^browser switchToPackages ].

	browser packageSelection actualObjects ifNotEmpty: [ :packages |
		browser switchToPackageScopeOf: packages]
]

{ #category : #testing }
ClySwitchToPackagesCommand >> isAppliedToBrowser [

	^browser packageView showsItemsFromQuery: ClyAllPackages
]
