"
I represent a package change in the system.
"
Class {
	#name : 'EpPackageChange',
	#superclass : 'EpCodeChange',
	#instVars : [
		'packageName'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'testing' }
EpPackageChange class >> isAbstract [

	^ self == EpPackageChange
]

{ #category : 'accessing' }
EpPackageChange class >> packageName: aPackageName [

	^ self new
		  packageName: aPackageName;
		  yourself
]

{ #category : 'accessing' }
EpPackageChange >> affectedPackageName [

	^ self packageName
]

{ #category : 'testing' }
EpPackageChange >> doesOverride: aCodeChange [

	^ aCodeChange isEpPackageChange and: [ aCodeChange packageName = self packageName ]
]

{ #category : 'testing' }
EpPackageChange >> isEpPackageChange [

	^ true
]

{ #category : 'accessing' }
EpPackageChange >> packageName [

	^ packageName
]

{ #category : 'accessing' }
EpPackageChange >> packageName: anObject [

	packageName := anObject
]
