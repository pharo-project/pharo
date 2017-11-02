"
Example for a package method node
"
Class {
	#name : #PackageMethodCategoryNodeExample,
	#superclass : #PackageAbstractNodeExample,
	#category : 'Morphic-Examples-MorphTreeWidget'
}

{ #category : #accessing }
PackageMethodCategoryNodeExample >> childNodeClassFromItem: anItem [
	^ PackageMethodNodeExample
]

{ #category : #accessing }
PackageMethodCategoryNodeExample >> childrenItems [
	^ self parentNode item selectorsInProtocol: self item
]
