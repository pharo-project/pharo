"
I am a command to switch full browser to raw packages mode. 
"
Class {
	#name : 'ClySwitchToPackagesCommand',
	#superclass : 'ClySwitchPackageViewModeCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Packages',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Packages'
}

{ #category : 'activation' }
ClySwitchToPackagesCommand class >> toolbarOrder [
	^1
]

{ #category : 'accessing' }
ClySwitchToPackagesCommand >> description [
	^ 'Click to show all packages'
]

{ #category : 'execution' }
ClySwitchToPackagesCommand >> executeRefactoring [

	^ self browser switchToPackages
]

{ #category : 'testing' }
ClySwitchToPackagesCommand >> isAppliedToBrowser [

	^ (self browser packageView showsItemsFromQuery: ClyAllPackagesQuery) and: [ self browser isScopedModeEnabled not ]
]

{ #category : 'accessing' }
ClySwitchToPackagesCommand >> name [

	^ 'All Packages'
]
