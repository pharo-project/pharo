"
I'm a group node model for the package groups
"
Class {
	#name : #PackageTreePackageGroupNodeModel,
	#superclass : #PackageTreeGroupModel,
	#category : 'Nautilus-Tree'
}

{ #category : #testing }
PackageTreePackageGroupNodeModel >> canBeRenamed [
	^ true
]

{ #category : #accessing }
PackageTreePackageGroupNodeModel >> childNodeClassFromItem: anItem [
	^ PackageTreePackageNodeModel
]

{ #category : #accessing }
PackageTreePackageGroupNodeModel >> childrenItems [
	^ self item elements
]

{ #category : #'event handling' }
PackageTreePackageGroupNodeModel >> doubleClick [
	self model model renameGroup
]

{ #category : #accessing }
PackageTreePackageGroupNodeModel >> icon [
	^ self iconNamed: #publishIcon
]
