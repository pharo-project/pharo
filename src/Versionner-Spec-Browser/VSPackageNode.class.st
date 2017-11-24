"
Class used to display packages in a tree.
"
Class {
	#name : #VSPackageNode,
	#superclass : #TreeNodePresenter,
	#category : #'Versionner-Spec-Browser'
}

{ #category : #accessing }
VSPackageNode >> childNodeClassFor: anItem [
	^ VSDetailsLeafNode
]

{ #category : #accessing }
VSPackageNode >> children [
	^ [ self content dependencies collect: [ :each | 'requires: ' , each] ]
]

{ #category : #testing }
VSPackageNode >> hasChildren [
	^ self content dependencies isNotEmpty
]

{ #category : #accessing }
VSPackageNode >> icon [
	^  self iconNamed: #packageColoredIcon
]

{ #category : #accessing }
VSPackageNode >> textMorph [
	^ self theme newTextIn: World text: self item asString
]
