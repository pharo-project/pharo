Class {
	#name : 'ClySwitchToScopedViewCommand',
	#superclass : 'ClySwitchPackageViewModeCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Packages',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Packages'
}

{ #category : 'activation' }
ClySwitchToScopedViewCommand class >> toolbarOrder [
	^1
]

{ #category : 'accessing' }
ClySwitchToScopedViewCommand >> description [

	^ 'Scope to selected packages'
]

{ #category : 'execution' }
ClySwitchToScopedViewCommand >> executeRefactoring [

	^ self browser packageSelection actualObjects ifNotEmpty: [ :packages |
		self browser switchToPackageScopeOf: packages]
]

{ #category : 'testing' }
ClySwitchToScopedViewCommand >> isAppliedToBrowser [

	^ (self browser packageView showsItemsFromQuery: ClyAllPackagesQuery) and: [ self browser isScopedModeEnabled ]
]

{ #category : 'accessing' }
ClySwitchToScopedViewCommand >> name [

	^ 'Scoped View'
]
