"
Abstract class used to display a package with its specific icons and no child in a tree.
"
Class {
	#name : #VSPackageLeafNode,
	#superclass : #VSPackageNode,
	#category : #'Versionner-Spec-Browser'
}

{ #category : #accessing }
VSPackageLeafNode >> children [
	^ [ #() ]

]

{ #category : #testing }
VSPackageLeafNode >> hasChildren [
	^ false
]
