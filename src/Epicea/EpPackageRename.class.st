"
I am a change representing the renaming of a package.
"
Class {
	#name : 'EpPackageRename',
	#superclass : 'EpPackageChange',
	#instVars : [
		'oldPackageName'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpPackageRename class >> oldName: aString newName: anotherString [

	^ (self packageName: anotherString)
		  oldPackageName: aString;
		  yourself
]

{ #category : 'visitor' }
EpPackageRename >> accept: aVisitor [

	^ aVisitor visitPackageRename: self
]

{ #category : 'accessing' }
EpPackageRename >> newPackageName [

	^ self packageName
]

{ #category : 'accessing' }
EpPackageRename >> oldPackageName [

	^ oldPackageName
]

{ #category : 'accessing' }
EpPackageRename >> oldPackageName: anObject [

	oldPackageName := anObject
]
