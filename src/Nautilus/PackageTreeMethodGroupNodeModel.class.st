"
I'm a group node model for the last modified methods group.
"
Class {
	#name : #PackageTreeMethodGroupNodeModel,
	#superclass : #PackageTreeGroupModel,
	#category : #'Nautilus-Tree'
}

{ #category : #accessing }
PackageTreeMethodGroupNodeModel >> icon [ 
	^ self model groupIconFor: self item.
]
