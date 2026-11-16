"
I am an info about a requirement whose baseline could ask for a newer version (see MetacelloMVSResolver>>reportUpgrades).

project: the name of the project asked (Hashtable).
asked: what the baseline asks after the repository, as written (v1.x.x), or 'no version' when it names none.
version: the version string I propose to ask for, as a baseline would write it (v1.2.1).
"
Class {
	#name : 'MetacelloMVSUpgradeFinding',
	#superclass : 'MetacelloResolutionFinding',
	#instVars : [
		'project',
		'asked',
		'version'
	],
	#category : 'Metacello-MVS',
	#package : 'Metacello-MVS'
}

{ #category : 'accessing' }
MetacelloMVSUpgradeFinding >> asked [

	^ asked
]

{ #category : 'accessing' }
MetacelloMVSUpgradeFinding >> project [

	^ project
]

{ #category : 'accessing' }
MetacelloMVSUpgradeFinding >> project: aProjectName asked: aString version: aVersionString [

	project := aProjectName.
	asked := aString.
	version := aVersionString
]

{ #category : 'accessing' }
MetacelloMVSUpgradeFinding >> version [

	^ version
]
