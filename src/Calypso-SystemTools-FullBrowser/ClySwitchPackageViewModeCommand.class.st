"
I am a base class for commands which switch full browser package mode
It can be either raw packages or projects
"
Class {
	#name : 'ClySwitchPackageViewModeCommand',
	#superclass : 'ClySwitchBrowserModeCommand',
	#category : 'Calypso-SystemTools-FullBrowser-Commands-Packages',
	#package : 'Calypso-SystemTools-FullBrowser',
	#tag : 'Commands-Packages'
}

{ #category : 'testing' }
ClySwitchPackageViewModeCommand class >> isAbstract [
	^self = ClySwitchPackageViewModeCommand
]

{ #category : 'activation' }
ClySwitchPackageViewModeCommand class >> toolbarOrder [
	<classAnnotationDependency>
	^10
]

{ #category : 'testing' }
ClySwitchPackageViewModeCommand >> isAppliedToBrowser [
	^false
]
