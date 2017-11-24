"
I'm a tag node model.
"
Class {
	#name : #PackageTreeTagNodeModel,
	#superclass : #PackageTreeNodeModel,
	#category : #'Nautilus-Tree'
}

{ #category : #converting }
PackageTreeTagNodeModel >> asNautilusSelection [ 
	^ PackageTreeTagSelection node: self
]

{ #category : #testing }
PackageTreeTagNodeModel >> canBeRenamed [
	^ true
]

{ #category : #accessing }
PackageTreeTagNodeModel >> icon [ 
	^ self model tagIconFor: self item
]

{ #category : #accessing }
PackageTreeTagNodeModel >> rowMorphForColumn: aTreeColumn [
	^ self item name asMorph
]
