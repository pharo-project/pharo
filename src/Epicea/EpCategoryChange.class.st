"
I represent a change in the categories of a package.
"
Class {
	#name : #EpCategoryChange,
	#superclass : #EpCodeChange,
	#instVars : [
		'classCategoryName',
		'affectedPackageName',
		'packageTagName'
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

	^ (self packageOrganizer packageMatchingExtensionName: aName)
		  ifNotNil: [ :package | package name ]
		  ifNil: [ aName ]
]

{ #category : #'instance creation' }
EpCategoryChange class >> tagName: aName packageName: aPackageName [

	^ self new
		  packageTagName: aName;
		  affectedPackageName: aPackageName;
		  yourself
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

	self flag: #package. "We should not rely on categories"
	^ classCategoryName ifNil: [
		  self packageTagName = self affectedPackageName
			  ifTrue: [ self affectedPackageName ]
			  ifFalse: [ self affectedPackageName , '-' , self packageTagName ] ]
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
EpCategoryChange >> packageTagName [

	^ packageTagName
]

{ #category : #accessing }
EpCategoryChange >> packageTagName: anObject [

	packageTagName := anObject
]
