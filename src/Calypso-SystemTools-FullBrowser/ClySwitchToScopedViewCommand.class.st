Class {
	#name : #ClySwitchToScopedViewCommand,
	#superclass : #ClySwitchPackageViewModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Packages'
}

{ #category : #activation }
ClySwitchToScopedViewCommand class >> toolbarOrder [
	^1
]

{ #category : #accessing }
ClySwitchToScopedViewCommand >> defaultMenuItemName [
	
	^ 'Scoped View' 
]

{ #category : #accessing }
ClySwitchToScopedViewCommand >> description [

	^ 'Scope to selected packages'.
]

{ #category : #execution }
ClySwitchToScopedViewCommand >> execute [
	
	^ browser packageSelection actualObjects ifNotEmpty: [ :packages |
		browser switchToPackageScopeOf: packages]
]

{ #category : #testing }
ClySwitchToScopedViewCommand >> isAppliedToBrowser [

	^ (browser packageView showsItemsFromQuery: ClyAllPackagesQuery) and: [ browser isScopedModeEnabled ]
]
