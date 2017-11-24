"
I'm a base selection of an item from package tree. 

"
Class {
	#name : #PackageTreeSelection,
	#superclass : #Object,
	#instVars : [
		'node'
	],
	#category : #'Nautilus-Tree'
}

{ #category : #'instance creation' }
PackageTreeSelection class >> new [ 
	self error: 'Use #node:'
]

{ #category : #'instance creation' }
PackageTreeSelection class >> node: aNode [ 
	^ self basicNew 
		initializeNode: aNode;
		yourself
]

{ #category : #comparing }
PackageTreeSelection >> = other [ 
	^ (self class = other class) and:[ self item = other item].

]

{ #category : #converting }
PackageTreeSelection >> asNautilusSelection [
	^ self
]

{ #category : #accessing }
PackageTreeSelection >> categoryName [ 
	^ self subclassResponsibility
]

{ #category : #accessing }
PackageTreeSelection >> definedClasses [
	^ self subclassResponsibility
]

{ #category : #accessing }
PackageTreeSelection >> extendedClasses [
	^ #()
]

{ #category : #accessing }
PackageTreeSelection >> extensionClasses [
	^ #()
]

{ #category : #comparing }
PackageTreeSelection >> hash [
	^ self class hash bitXor: self item hash
]

{ #category : #testing }
PackageTreeSelection >> includesCategory: aSymbol [
	^ false
]

{ #category : #testing }
PackageTreeSelection >> includesClass: aClass [ 
	^ self subclassResponsibility
]

{ #category : #initialization }
PackageTreeSelection >> initializeNode: aNode [ 
	self initialize.
	node := aNode. 
]

{ #category : #testing }
PackageTreeSelection >> isGroup [ 
	^ false
]

{ #category : #testing }
PackageTreeSelection >> isPackageTag [

	^ false
]

{ #category : #accessing }
PackageTreeSelection >> item [ 
	^ self node item.
]

{ #category : #accessing }
PackageTreeSelection >> mcPackage [ 
	^ self package mcPackage
]

{ #category : #accessing }
PackageTreeSelection >> menu: aMenu shifted: aBoolean target: aTarget [
	^ self subclassResponsibility
]

{ #category : #accessing }
PackageTreeSelection >> name [ 
	^ self item name
]

{ #category : #accessing }
PackageTreeSelection >> node [ 
	^ node
]

{ #category : #converting }
PackageTreeSelection >> nodePath [ 
	^ self node path
]

{ #category : #accessing }
PackageTreeSelection >> registerSelectionTarget: aTarget [ 
	self subclassResponsibility
]

{ #category : #accessing }
PackageTreeSelection >> removeTarget: target [ 
	self subclassResponsibility.
]

{ #category : #accessing }
PackageTreeSelection >> renameTarget: target [ 
]

{ #category : #converting }
PackageTreeSelection >> withClassesIn: anEnvironment createHierarchySorter: aHierarchyClass [ 
	| classes defined extensions |
	classes := self definedClasses collect: #theNonMetaClass.
	anEnvironment isSystem
		ifFalse: [ classes := classes intersection: anEnvironment classes ].

	defined := aHierarchyClass buildHierarchyForClasses: classes.
	
	classes := self extensionClasses collect: #theNonMetaClass.
	anEnvironment isSystem
		ifFalse: [ classes := classes intersection: anEnvironment classes ].
	
	extensions := aHierarchyClass buildHierarchyForClasses: classes asSet.

	^ defined, extensions
]
