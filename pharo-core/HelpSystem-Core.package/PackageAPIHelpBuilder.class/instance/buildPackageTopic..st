building
buildPackageTopic: pTopic
 
	| classTopic classes |
	classes := (RPackageSet named: pTopic title) classes 
		asSortedCollection: [:cl1 :cl2 | cl1 name < cl2 name].
	classes do: [ :aClass | 
		classTopic := ClassAPIHelpBuilder 
			buildHierarchicalHelpTopicFrom: aClass 
			withSubclasses: false 
			withMethods: true.
		pTopic addSubtopic: classTopic ]