"
I represent the rename of a category in a package.
"
Class {
	#name : #EpPackageTagRename,
	#superclass : #EpCategoryChange,
	#instVars : [
		'oldTagName'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpPackageTagRename class >> oldName: oldName newName: newName packageName: packageName [

	^ (self tagName: newName packageName: packageName)
		  oldTagName: oldName;
		  yourself
]

{ #category : #visitor }
EpPackageTagRename >> accept: aVisitor [
	^ aVisitor visitCategoryRename: self
]

{ #category : #accessing }
EpPackageTagRename >> newCategoryName [

	^ self categoryName
]

{ #category : #accessing }
EpPackageTagRename >> newTagName [

	^ self tagName
]

{ #category : #accessing }
EpPackageTagRename >> oldCategoryName [

	self flag: #package. "We should not rely on categories"
	^ self oldTagName = self affectedPackageName
		  ifTrue: [ self affectedPackageName ]
		  ifFalse: [ self affectedPackageName , '-' , self oldTagName ]
]

{ #category : #accessing }
EpPackageTagRename >> oldTagName [

	^ oldTagName
]

{ #category : #accessing }
EpPackageTagRename >> oldTagName: anObject [

	oldTagName := anObject
]
