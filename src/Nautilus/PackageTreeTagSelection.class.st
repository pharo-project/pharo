"
I'm a tag selection.
"
Class {
	#name : #PackageTreeTagSelection,
	#superclass : #PackageTreePackageSelection,
	#category : #Nautilus-Tree
}

{ #category : #'instance creation' }
PackageTreeTagSelection class >> tag: aTag [ 
	^ self node: ((PackageTreeTagNodeModel with: aTag)
		parentNode: (PackageTreePackageNodeModel with: aTag package);
		yourself)
]

{ #category : #accessing }
PackageTreeTagSelection >> classTags [ 
	^ #()
]

{ #category : #accessing }
PackageTreeTagSelection >> classes [ 
	^ self definedClasses
]

{ #category : #accessing }
PackageTreeTagSelection >> definedClasses [ 
	^ self item classes
]

{ #category : #accessing }
PackageTreeTagSelection >> extendedClasses [
	^ #()
]

{ #category : #accessing }
PackageTreeTagSelection >> extensionClasses [
	^ #()
]

{ #category : #testing }
PackageTreeTagSelection >> includesCategory: aSymbol [
	^ self categoryName asSymbol == aSymbol
]

{ #category : #testing }
PackageTreeTagSelection >> includesClass: aClass [ 
	^ self item classNames includes: aClass instanceSide name.
]

{ #category : #testing }
PackageTreeTagSelection >> isPackageTag [

	^ true
]

{ #category : #accessing }
PackageTreeTagSelection >> package [ 
	^ self item package
]

{ #category : #accessing }
PackageTreeTagSelection >> removeTarget: target [ 
	target removeClassCategory: self.
]

{ #category : #accessing }
PackageTreeTagSelection >> renameTarget: target [ 
	target renameClassCategory: self item.
]
