"
Class used to display projects with specific icons and no child in a tree.
"
Class {
	#name : #VSProjectNode,
	#superclass : #TreeNodePresenter,
	#category : #Versionner-Spec-Browser
}

{ #category : #accessing }
VSProjectNode >> childNodeClassFor: anItem [
	^ VSDetailsLeafNode
]

{ #category : #accessing }
VSProjectNode >> children [
	^ [ self content dependencies collect: [ :each | 'loads: ' , each] ]
]

{ #category : #testing }
VSProjectNode >> hasChildren [
	^ self content dependencies isNotEmpty
]

{ #category : #accessing }
VSProjectNode >> icon [
	 
	^ VersionnerSpecBrowser configurationLoadedIcon
]
