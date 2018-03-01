"
A builder for the API of the various packages
"
Class {
	#name : #PackageAPIHelpBuilder,
	#superclass : #HelpBuilder,
	#category : #'HelpSystem-Core-Builders'
}

{ #category : #building }
PackageAPIHelpBuilder >> build [ 
	|pTopic|
	topicToBuild := (HelpTopic named: rootToBuildFrom bookName).
	rootToBuildFrom helpPackages do: [:package|
		pTopic := HelpTopic named: package.
		topicToBuild addSubtopic: pTopic.
		self buildPackageTopic: pTopic.
		
	]

]

{ #category : #building }
PackageAPIHelpBuilder >> buildPackageTopic: pTopic [
 
	| classTopic classes |
	classes := (RPackageSet named: pTopic title) classes 
		asSortedCollection: [:cl1 :cl2 | cl1 name < cl2 name].
	classes do: [ :aClass | 
		classTopic := ClassAPIHelpBuilder 
			buildHierarchicalHelpTopicFrom: aClass 
			withSubclasses: false 
			withMethods: true.
		pTopic addSubtopic: classTopic ]
]
