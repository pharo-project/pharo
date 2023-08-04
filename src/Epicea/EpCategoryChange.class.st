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
EpCategoryChange class >> named: aName packageName: aPackageName [

	^ self new
		  classCategoryName: aName;
		  affectedPackageName: aPackageName;
		  yourself
]

{ #category : #'instance creation' }
EpCategoryChange class >> tagName: aName packageName: aPackageName [

	^ self new
		  tagName: aName;
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
		  self tagName = self affectedPackageName
			  ifTrue: [ self affectedPackageName ]
			  ifFalse: [ self affectedPackageName , '-' , self tagName ] ]
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

	^ packageTagName
]

{ #category : #accessing }
EpCategoryChange >> tagName: aString [

	packageTagName := aString
]
