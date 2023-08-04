"
I represent a change in the categories of a package.
"
Class {
	#name : #EpPackageTagChange,
	#superclass : #EpCodeChange,
	#instVars : [
		'affectedPackageName',
		'packageTagName'
	],
	#category : #'Epicea-Model'
}

{ #category : #'instance creation' }
EpPackageTagChange class >> tagName: aName packageName: aPackageName [

	^ self new
		  tagName: aName;
		  affectedPackageName: aPackageName;
		  yourself
]

{ #category : #accessing }
EpPackageTagChange >> affectedPackageName [

	^ affectedPackageName
]

{ #category : #accessing }
EpPackageTagChange >> affectedPackageName: anObject [

	affectedPackageName := anObject
]

{ #category : #accessing }
EpPackageTagChange >> categoryName [

	self flag: #package. "We should not rely on categories"
	^ self tagName = self affectedPackageName
		  ifTrue: [ self affectedPackageName ]
		  ifFalse: [ self affectedPackageName , '-' , self tagName ]
]

{ #category : #testing }
EpPackageTagChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpCategoryChange and: [
		aCodeChange affectedPackageName = self affectedPackageName ]
]

{ #category : #testing }
EpPackageTagChange >> isEpCategoryChange [

	^ true
]

{ #category : #accessing }
EpPackageTagChange >> tagName [

	^ packageTagName
]

{ #category : #accessing }
EpPackageTagChange >> tagName: aString [

	packageTagName := aString
]
