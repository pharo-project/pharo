"
I'm a package node model.
"
Class {
	#name : #PackageTreePackageNodeModel,
	#superclass : #PackageTreeNodeModel,
	#category : #Nautilus-Tree
}

{ #category : #converting }
PackageTreePackageNodeModel >> asNautilusSelection [
	^ PackageTreePackageSelection node: self
]

{ #category : #testing }
PackageTreePackageNodeModel >> canBeRenamed [
	^ true
]

{ #category : #accessing }
PackageTreePackageNodeModel >> childNodeClassFromItem: anItem [ 
	^ PackageTreeTagNodeModel
]

{ #category : #accessing }
PackageTreePackageNodeModel >> childrenItems [
	| tags |

	tags := self item classTags.
	"If there is just one, do not show"
	^ (tags size = 1 and: [ tags anyOne name = self item name ])
		ifTrue: [ #() ]
		ifFalse: [ tags sorted: [ :a :b | a categoryName < b categoryName ] ]
]

{ #category : #accessing }
PackageTreePackageNodeModel >> helpText [
	^ self model helpTextFor: self item.
]

{ #category : #accessing }
PackageTreePackageNodeModel >> icon [ 
	^ self model packageIconFor: self item.
]

{ #category : #accessing }
PackageTreePackageNodeModel >> rowMorphForColumn: aTreeColumn [
	^ self item name asMorph
]
