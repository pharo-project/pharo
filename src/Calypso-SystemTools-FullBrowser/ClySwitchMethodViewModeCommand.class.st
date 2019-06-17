"
I am a base class for commands which switch full browser method group mode.
It can be either method groups or variables.

Subclasses define what kind of method groups whey target. They implement following class side method: 

- methodGroupType
"
Class {
	#name : #ClySwitchMethodViewModeCommand,
	#superclass : #ClySwitchBrowserModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MethodGroups'
}

{ #category : #testing }
ClySwitchMethodViewModeCommand class >> canBeExecutedInContext: aToolContext [
	^aToolContext isClassSelected
]

{ #category : #activation }
ClySwitchMethodViewModeCommand class >> fullBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation byItemOf: ClyMethodViewToolbarGroup for: ClyClassContextOfFullBrowser 
]

{ #category : #testing }
ClySwitchMethodViewModeCommand class >> isAbstract [
	^self = ClySwitchMethodViewModeCommand 
]

{ #category : #accessing }
ClySwitchMethodViewModeCommand >> defaultMenuItemName [
	^self methodGroupType shortName
]

{ #category : #testing }
ClySwitchMethodViewModeCommand >> isAppliedToBrowser [
	^browser methodGroupView showsItemsOfType: self methodGroupType
]

{ #category : #accessing }
ClySwitchMethodViewModeCommand >> methodGroupType [
	self subclassResponsibility 
]
