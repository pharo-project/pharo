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
	
	^ 'All Packages' 
]

{ #category : #accessing }
ClySwitchToPackagesCommand >> description [
	^ 'Click to show all packages'.
]

{ #category : #execution }
ClySwitchToPackagesCommand >> execute [
	^browser switchToPackages
]

{ #category : #testing }
ClySwitchToPackagesCommand >> isAppliedToBrowser [

	^ (browser packageView showsItemsFromQuery: ClyAllPackagesQuery) and: [ browser isScopedModeEnabled not ]
]
