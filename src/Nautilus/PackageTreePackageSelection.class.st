"
I'm a package selection.
"
Class {
	#name : #PackageTreePackageSelection,
	#superclass : #PackageTreeSelection,
	#category : #Nautilus-Tree
}

{ #category : #'instance creation' }
PackageTreePackageSelection class >> package: aPackage [ 
	^ self node: (PackageTreePackageNodeModel with: aPackage)
]

{ #category : #accessing }
PackageTreePackageSelection >> categoryName [ 
	^ self item categoryName
]

{ #category : #accessing }
PackageTreePackageSelection >> classTags [ 
	^ self item classTags
]

{ #category : #accessing }
PackageTreePackageSelection >> classes [ 
	^ self item classes
]

{ #category : #accessing }
PackageTreePackageSelection >> definedClasses [ 
	^ self item definedClasses
]

{ #category : #accessing }
PackageTreePackageSelection >> extendedClasses [
	^ self item extendedClasses
]

{ #category : #'system compatibility' }
PackageTreePackageSelection >> extensionCategoriesForClass: aClass [
	^ self item extensionCategoriesForClass: aClass
]

{ #category : #accessing }
PackageTreePackageSelection >> extensionClasses [
	^ self item extensionClasses
]

{ #category : #testing }
PackageTreePackageSelection >> includesCategory: aSymbol [
	^ self package name == aSymbol
]

{ #category : #testing }
PackageTreePackageSelection >> includesClass: aClass [ 
	^ self item includesClass: aClass
]

{ #category : #testing }
PackageTreePackageSelection >> includesSelector: aSelector ofClass: aClass [
	^ self item includesSelector: aSelector ofClass: aClass
]

{ #category : #accessing }
PackageTreePackageSelection >> menu: aMenu shifted: aBoolean target: aTarget [
	^ aTarget packagesMenu: aMenu shifted: aBoolean
]

{ #category : #accessing }
PackageTreePackageSelection >> package [ 
	^ self item
]

{ #category : #accessing }
PackageTreePackageSelection >> packageComment [
	^ self item package packageComment
]

{ #category : #accessing }
PackageTreePackageSelection >> packageComment: aDescription [
	^ self item package packageComment: aDescription
]

{ #category : #accessing }
PackageTreePackageSelection >> packageName [
	^ self item packageName
]

{ #category : #accessing }
PackageTreePackageSelection >> registerSelectionTarget: aTarget [ 
	aTarget registerSelectionPackage: self.
]

{ #category : #accessing }
PackageTreePackageSelection >> removeTarget: target [ 
	target removePackageCategory: self.
]

{ #category : #accessing }
PackageTreePackageSelection >> renameTarget: target [   
	target renamePackageCategory: self item.
]
