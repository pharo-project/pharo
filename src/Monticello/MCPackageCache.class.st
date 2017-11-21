"
I'm a kind of cache for versions and filenames of packages.
"
Class {
	#name : #MCPackageCache,
	#superclass : #Object,
	#instVars : [
		'sorter',
		'fileNames'
	],
	#category : #Monticello-Repositories
}

{ #category : #initialization }
MCPackageCache >> initialize [
	super initialize.
	sorter := MCVersionSorter new.
	fileNames := Dictionary new.
]

{ #category : #accessing }
MCPackageCache >> recordVersionInfo: aVersionInfo forFileNamed: aString [
	fileNames at: aVersionInfo put: aString.
	sorter addVersionInfo: aVersionInfo
]

{ #category : #accessing }
MCPackageCache >> versionInfos [
	^ sorter sortedVersionInfos 
]
