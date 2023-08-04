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

{ #category : #testing }
EpPackageTagChange class >> isAbstract [

	^ self == EpPackageTagChange
]

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

{ #category : #testing }
EpPackageTagChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpPackageTagChange and: [
		aCodeChange affectedPackageName = self affectedPackageName ]
]

{ #category : #testing }
EpPackageTagChange >> isEpPackageTagChange [

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
