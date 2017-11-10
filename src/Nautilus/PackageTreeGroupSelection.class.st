"
I'm a group selection for group.
"
Class {
	#name : #PackageTreeGroupSelection,
	#superclass : #PackageTreeSelection,
	#category : #Nautilus-Tree
}

{ #category : #'instance creation' }
PackageTreeGroupSelection class >> group: aGroup [
	^ self node: (PackageTreeMethodGroupNodeModel with: aGroup)
]

{ #category : #accessing }
PackageTreeGroupSelection >> categoryName [ 
	^ ''
]

{ #category : #accessing }
PackageTreeGroupSelection >> classes [
	^ self definedClasses
]

{ #category : #accessing }
PackageTreeGroupSelection >> definedClasses [
	^ self item classes
]

{ #category : #testing }
PackageTreeGroupSelection >> includesCategory: aSymbol [
	^ self item packages anySatisfy: [ :each | each name == aSymbol ]
]

{ #category : #testing }
PackageTreeGroupSelection >> includesClass: aClass [
	^ self item hasClassNamed: aClass name
]

{ #category : #testing }
PackageTreeGroupSelection >> isGroup [ 
	^ true
]

{ #category : #testing }
PackageTreeGroupSelection >> isReadOnly [ 
	^ self item isReadOnly 
]

{ #category : #accessing }
PackageTreeGroupSelection >> menu: aMenu shifted: aBoolean target: aTarget [
	^ aTarget groupsMenu: aMenu shifted: aBoolean
]

{ #category : #accessing }
PackageTreeGroupSelection >> methodsFor: class [ 
	^ self item methodsFor: class
]

{ #category : #accessing }
PackageTreeGroupSelection >> methodsFor: class categorised: aCategory [
	^ self item 
		methodsFor: class 
		categorised: aCategory
]

{ #category : #accessing }
PackageTreeGroupSelection >> package [ 
	^ self item packages 
		ifNotEmpty: [ :packages | packages first ]
]

{ #category : #accessing }
PackageTreeGroupSelection >> protocolsFor: class [ 
	^ self item protocolsFor: class
]

{ #category : #accessing }
PackageTreeGroupSelection >> registerSelectionTarget: aTarget [ 
	aTarget registerSelectionGroup: self.
]

{ #category : #testing }
PackageTreeGroupSelection >> removable [ 
	^ self item removable 
]
