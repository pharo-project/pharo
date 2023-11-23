"
I represent the rename of a category in a package.
"
Class {
	#name : 'EpPackageTagRename',
	#superclass : 'EpPackageTagChange',
	#instVars : [
		'oldTagName'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpPackageTagRename class >> oldName: oldName newName: newName packageName: packageName [

	^ (self tagName: newName packageName: packageName)
		  oldTagName: oldName;
		  yourself
]

{ #category : 'visitor' }
EpPackageTagRename >> accept: aVisitor [
	^ aVisitor visitPackageTagRename: self
]

{ #category : 'accessing' }
EpPackageTagRename >> newTagName [

	^ self tagName
]

{ #category : 'accessing' }
EpPackageTagRename >> oldTagName [

	^ oldTagName
]

{ #category : 'accessing' }
EpPackageTagRename >> oldTagName: anObject [

	oldTagName := anObject
]
