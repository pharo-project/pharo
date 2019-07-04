"
I am a base class for the commands which switch the full browser mode of the class view.
It can be flat list or class hierarchy for example
"
Class {
	#name : #ClySwitchClassViewModeCommand,
	#superclass : #ClySwitchBrowserModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-Hierarchy'
}

{ #category : #testing }
ClySwitchClassViewModeCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected
]

{ #category : #activation }
ClySwitchClassViewModeCommand class >> fullBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyClassHierarchyToolbarGroup for: ClyClassContextOfFullBrowser 
]

{ #category : #testing }
ClySwitchClassViewModeCommand class >> isAbstract [
	^self = ClySwitchClassViewModeCommand 
]
