"
I represent local scope of classes which merges both instance and class sides
"
Class {
	#name : #ClyBothMetaLevelClassScope,
	#superclass : #ClyLocalClassScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #'meta level' }
ClyBothMetaLevelClassScope class >> metaLevelForHierarchyOf: aClass [
	"For hierarchies I work as ClyMetaLevelClassScope.
	I restrict any hierarchy by instance side relationship.
	For example using class side scope you can analyze hierarchy of ProtoObject. 
	It will gives you only ProtoObject and ProtoObject class despite of
	ProtoObject class superclass is not nil"
	^aClass instanceSide 
]

{ #category : #'meta level' }
ClyBothMetaLevelClassScope class >> metaLevelsOf: aClass do: aBlock [

	aBlock value: aClass instanceSide.
	aBlock value: aClass classSide
]

{ #category : #queries }
ClyBothMetaLevelClassScope >> classesDo: aBlock [

	basisObjects do: [ :each | 
		aBlock value: each instanceSide ]
]
