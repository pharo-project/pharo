"
I am a menu item that renders only a line to separate plugins from normal actions.
"
Class {
	#name : #IcePluginSeparatorItem,
	#superclass : #IceTipCommand,
	#category : #'Iceberg-Plugin-Base'
}

{ #category : #testing }
IcePluginSeparatorItem class >> browserContextActivation [
	<classAnnotation>
	
	^ CmdContextMenuActivation
			byRootGroupItemOrder: SmallInteger maxVal - 2000
			for: IceTipRepositoryListContext
]

{ #category : #testing }
IcePluginSeparatorItem class >> canBeExecutedInContext: aToolContext [

	^ true
]

{ #category : #execution }
IcePluginSeparatorItem >> execute [
	
	"Nothing"
]

{ #category : #accessing }
IcePluginSeparatorItem >> fillContextMenu: aMenu using: aCommandActivator [

	"Just add a line to separate plugins from normal actions"
	aMenu addLine
]
