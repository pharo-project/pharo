"
Example for a pakage node
"
Class {
	#name : #PackageNodeExample,
	#superclass : #PackageAbstractNodeExample,
	#category : #Morphic-Examples-MorphTreeWidget
}

{ #category : #accessing }
PackageNodeExample >> asString [
	^ self item packageName
]

{ #category : #accessing }
PackageNodeExample >> childNodeClassFromItem: anItem [
	^  PackageClassNodeExample 
]

{ #category : #accessing }
PackageNodeExample >> childrenItems [
	^ self item classes 
]
