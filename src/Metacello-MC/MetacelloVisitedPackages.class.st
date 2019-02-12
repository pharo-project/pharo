Class {
	#name : #MetacelloVisitedPackages,
	#superclass : #Object,
	#instVars : [
		'groups',
		'packages',
		'projects'
	],
	#category : #'Metacello-MC-Specs'
}

{ #category : #initialization }
MetacelloVisitedPackages >> initialize [

	groups := Set new.
	packages := Set new.
	projects := Set new.
]

{ #category : #visiting }
MetacelloVisitedPackages >> pushProject: aBlock [

	| oldGroups oldPackages oldProjects |
	oldGroups := groups.
	oldPackages := packages.
	oldProjects := projects.
	groups := Set new.
	packages := Set new.
	^aBlock ensure: [
		groups := oldGroups.
		packages := oldPackages.
		projects := oldProjects ]
]

{ #category : #visiting }
MetacelloVisitedPackages >> visit: aSpec doing: aBlock [

	aSpec
		projectDo: [:spec | 
			(projects includes: spec name) ifTrue: [ ^self ].
			projects add: spec name ] 
		packageDo: [:spec | 
			(packages includes: spec name) ifTrue: [ ^self ].
			packages add: spec name ] 
		groupDo: [:spec | 
			(groups includes: spec name) ifTrue: [ ^self ].
			groups add: spec name ].
	aBlock value: aSpec
]
