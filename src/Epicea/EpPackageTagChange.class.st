"
I represent a change in the categories of a package.
"
Class {
	#name : 'EpPackageTagChange',
	#superclass : 'EpCodeChange',
	#instVars : [
		'packageTagName',
		'packageName'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'testing' }
EpPackageTagChange class >> isAbstract [

	^ self == EpPackageTagChange
]

{ #category : 'instance creation' }
EpPackageTagChange class >> tagName: aName packageName: aPackageName [

	^ self new
		  tagName: aName;
		  packageName: aPackageName;
		  yourself
]

{ #category : 'accessing' }
EpPackageTagChange >> affectedName [

	^ self packageName
]

{ #category : 'accessing' }
EpPackageTagChange >> affectedPackageName [

	^ packageName
]

{ #category : 'testing' }
EpPackageTagChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpPackageTagChange and: [
		aCodeChange affectedPackageName = self affectedPackageName ]
]

{ #category : 'testing' }
EpPackageTagChange >> isEpPackageTagChange [

	^ true
]

{ #category : 'accessing' }
EpPackageTagChange >> packageName [

	^ packageName
]

{ #category : 'accessing' }
EpPackageTagChange >> packageName: aString [

	packageName := aString
]

{ #category : 'accessing' }
EpPackageTagChange >> tagName [

	^ packageTagName
]

{ #category : 'accessing' }
EpPackageTagChange >> tagName: aString [

	packageTagName := aString
]
