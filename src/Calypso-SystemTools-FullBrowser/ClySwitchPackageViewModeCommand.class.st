"
I am a base class for commands which switch full browser package mode
It can be either raw packages or projects
"
Class {
	#name : #ClySwitchPackageViewModeCommand,
	#superclass : #ClySwitchBrowserModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Packages'
}

{ #category : #activation }
ClySwitchPackageViewModeCommand class >> fullBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyPackageToolbarGroup 
		order: self toolbarOrder
		for: ClyPackageContextOfFullBrowser
]

{ #category : #activation }
ClySwitchPackageViewModeCommand class >> fullBrowserToolbarActivation2 [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyPackageToolbarGroup 
		order: self toolbarOrder
		for: ClyClassGroupContextOfFullBrowser
]

{ #category : #activation }
ClySwitchPackageViewModeCommand class >> fullBrowserToolbarActivation3 [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyPackageToolbarGroup 
		order: self toolbarOrder
		for: ClyProjectContextOfFullBrowser
]

{ #category : #testing }
ClySwitchPackageViewModeCommand class >> isAbstract [
	^self = ClySwitchPackageViewModeCommand 
]

{ #category : #activation }
ClySwitchPackageViewModeCommand class >> toolbarOrder [
	<classAnnotationDependency>
	^10
]

{ #category : #testing }
ClySwitchPackageViewModeCommand >> isAppliedToBrowser [
	^false
]
