"
I am an abstract group node model
"
Class {
	#name : #PackageTreeGroupModel,
	#superclass : #PackageTreeNodeModel,
	#category : #Nautilus-Tree
}

{ #category : #converting }
PackageTreeGroupModel >> asNautilusSelection [
	^ PackageTreeGroupSelection node: self
]

{ #category : #accessing }
PackageTreeGroupModel >> rowMorphForColumn: aTreeColumn [
	^ self item name asMorph
]
