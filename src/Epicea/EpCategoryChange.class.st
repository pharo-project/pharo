"
I represent a change in the categories of a package.
"
Class {
	#name : #EpCategoryChange,
	#superclass : #EpCodeChange,
	#instVars : [
		'classCategoryName',
		'affectedPackageName'
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

	^ classCategoryName
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
