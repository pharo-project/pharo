"
I represent a change in the categories of a package.
"
Class {
	#name : #EpCategoryChange,
	#superclass : #EpCodeChange,
	#instVars : [
		'classCategoryName',
		'affectedPackageName',
		'tagName'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpCategoryChange class >> named: aName [

	^ self named: aName packageName: (self packageNameFromCategoryName: aName)
]

{ #category : #'instance creation' }
EpCategoryChange class >> named: aName packageName: aPackageName [

	^ self new
		  classCategoryName: aName;
		  affectedPackageName: aPackageName;
		  yourself
]

{ #category : #'instance creation' }
EpCategoryChange class >> packageName: aPackageName tagName: aTagName [

	^ self new
		  tagName: aTagName;
		  affectedPackageName: aPackageName;
		  yourself
]

{ #category : #private }
EpCategoryChange class >> packageNameFromCategoryName: aName [
	^ (RPackage organizer packageMatchingExtensionName: aName) ifNotNil: [ :package | package name ] ifNil: [ aName ]
]

{ #category : #accessing }
EpCategoryChange >> affectedPackageName [

	^ affectedPackageName
]

{ #category : #accessing }
EpCategoryChange >> affectedPackageName: anObject [

	affectedPackageName := anObject
]

{ #category : #accessing }
EpCategoryChange >> categoryName [

	^ classCategoryName ifNil: [ self affectedPackageName , '-' , self tagName ]
]

{ #category : #accessing }
EpCategoryChange >> classCategoryName: anObject [

	classCategoryName := anObject
]

{ #category : #testing }
EpCategoryChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpCategoryChange and: [
		aCodeChange affectedPackageName = self affectedPackageName ]
]

{ #category : #testing }
EpCategoryChange >> isEpCategoryChange [

	^ true
]

{ #category : #accessing }
EpCategoryChange >> tagName [

	^ tagName
]

{ #category : #accessing }
EpCategoryChange >> tagName: anObject [

	tagName := anObject
]
