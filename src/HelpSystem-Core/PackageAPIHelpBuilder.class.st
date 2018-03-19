"
A builder for the API of the various packages
"
Class {
	#name : #PackageAPIHelpBuilder,
	#superclass : #HelpBuilder,
	#category : #'HelpSystem-Core-Builders'
}

{ #category : #'private - utilities' }
PackageAPIHelpBuilder class >> buildPackageTopic: packageTopic [
 
	| classTopic classes |
	classes := (RPackageSet named: packageTopic title) classes 
		asSortedCollection: [:class1 :class2 | class1 name < class2 name].
	classes do: [ :aClass | 
		classTopic := ClassAPIHelpBuilder 
			buildHierarchicalHelpTopicFrom: aClass 
			withSubclasses: false 
			withMethods: true.
		packageTopic addSubtopic: classTopic ]
]

{ #category : #building }
PackageAPIHelpBuilder >> build [ 
	| packageTopic |
	topicToBuild := (HelpTopic named: rootToBuildFrom bookName).
	rootToBuildFrom helpPackages do: [:package|
		packageTopic := HelpTopic named: package.
		topicToBuild addSubtopic: packageTopic.
		self class buildPackageTopic: packageTopic ]

]
