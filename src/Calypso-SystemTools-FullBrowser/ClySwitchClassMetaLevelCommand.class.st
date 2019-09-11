"
I am a base class for the commands which switch the full browser meta level.
Full browser can show instance side or class side of selected classes.

My subclasses define target meta level in following method: 

- metaLevelScope
It should return ClyInstanceSideScope or ClyClassSideScope.

Also I am annotated with toolbar activation strategy. So my subclasses will be shown in the browser toolbar as radio buttons.
And they should implement #toolbarOrder method

"
Class {
	#name : #ClySwitchClassMetaLevelCommand,
	#superclass : #ClySwitchBrowserModeCommand,
	#category : #'Calypso-SystemTools-FullBrowser-Commands-MetaLevel'
}

{ #category : #activation }
ClySwitchClassMetaLevelCommand class >> fullBrowserToolbarActivation [
	<classAnnotation>
	
	^ClyToolbarActivation 
		byItemOf: ClyMetaLevelToolbarGroup 
		order: self toolbarOrder 
		for: ClyClassContextOfFullBrowser 
]

{ #category : #testing }
ClySwitchClassMetaLevelCommand class >> isAbstract [
	^self = ClySwitchClassMetaLevelCommand 
]

{ #category : #activation }
ClySwitchClassMetaLevelCommand class >> toolbarOrder [
	<classAnnotationDependency>
	
	self subclassResponsibility 
]

{ #category : #accessing }
ClySwitchClassMetaLevelCommand >> defaultMenuItemName [
	^self metaLevelScope defaultName capitalized
]

{ #category : #execution }
ClySwitchClassMetaLevelCommand >> execute [
	browser switchToMetaLevelScope: self metaLevelScope
]

{ #category : #testing }
ClySwitchClassMetaLevelCommand >> isAppliedToBrowser [
	^browser metaLevelScope = self metaLevelScope
]

{ #category : #execution }
ClySwitchClassMetaLevelCommand >> metaLevelScope [
	self subclassResponsibility 
]
