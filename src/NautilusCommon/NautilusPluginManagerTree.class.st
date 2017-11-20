"
A NautilusPluginManagerTree is a tree which is used to render the plugins selection
"
Class {
	#name : #NautilusPluginManagerTree,
	#superclass : #MorphTreeModel,
	#instVars : [
		'model'
	],
	#category : #NautilusCommon-Plugin-Manager
}

{ #category : #'instance creation' }
NautilusPluginManagerTree class >> model: aModel [
	
	^ self new model: aModel
]

{ #category : #building }
NautilusPluginManagerTree >> buildPluginsTree [
	| tree | 
	tree := IdentityMorphTreeMorph on: self.
	tree
		listManager: (IdentityMorphTreeListManager new client: tree);
		columns: {
				MorphTreeColumn new startWidth: 100;
					rowMorphGetSelector: #firstMorph;
					headerButtonLabel: 'Plugin name' font:  nil.
				MorphTreeColumn new startWidth: 75;
					rowMorphGetSelector: #secondMorph;
					headerButtonLabel: 'Position' font:  nil};
		withHLines: true;
		beMultiple;
		autoDeselection: false;
		rowInset: 4;
		columnInset: 4;
		getMenuSelector: #menu:shifted:.

	^ tree
]

{ #category : #accessing }
NautilusPluginManagerTree >> model [

	^ model
]

{ #category : #accessing }
NautilusPluginManagerTree >> model: anObject [
	
	model := anObject
]

{ #category : #'t - accessing' }
NautilusPluginManagerTree >> rootItems [

	^ self model getPluginsList
]

{ #category : #'t - accessing' }
NautilusPluginManagerTree >> rootNodeClassFromItem: anItem [
	^ PluginTreeNode
]

{ #category : #'t - accessing' }
NautilusPluginManagerTree >> update [

	self updateList
]
