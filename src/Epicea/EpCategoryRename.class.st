"
I represent the rename of a category in a package.
"
Class {
	#name : #EpCategoryRename,
	#superclass : #EpCategoryChange,
	#instVars : [
		'oldCategoryName'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpCategoryRename class >> oldName: oldName newName: newName [

	^self
		oldName: oldName
		newName: newName
		packageName: (self packageNameFromCategoryName: newName)
]

{ #category : #'instance creation' }
EpCategoryRename class >> oldName: oldName newName: newName packageName: packageName [

	^ (self named: newName packageName: packageName)
		  oldCategoryName: oldName;
		  yourself
]

{ #category : #visitor }
EpCategoryRename >> accept: aVisitor [
	^ aVisitor visitCategoryRename: self
]

{ #category : #accessing }
EpCategoryRename >> newCategoryName [

	^ self categoryName
]

{ #category : #accessing }
EpCategoryRename >> oldCategoryName [

	^ oldCategoryName
]

{ #category : #accessing }
EpCategoryRename >> oldCategoryName: anObject [

	oldCategoryName := anObject
]
