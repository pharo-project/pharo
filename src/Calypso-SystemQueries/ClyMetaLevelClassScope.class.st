"
I am abstract scope which represents concrete meta level of class. There are instance side and class side scopes. 
Look at subclasses. They should implement class side method #resolveClass:  which returns instanceSide of classSide of given class
"
Class {
	#name : #ClyMetaLevelClassScope,
	#superclass : #ClyLocalClassScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #'meta level' }
ClyMetaLevelClassScope class >> metaLevelForHierarchyOf: aClass [
	"My subclasses should restrict any hierarchy by instance side relationship.
	For example using class side scope you can analyze hierarchy of ProtoObject. 
	It will gives you only ProtoObject class despite of
	ProtoObject class superclass is not nil"
	^aClass instanceSide 
]

{ #category : #'meta level' }
ClyMetaLevelClassScope class >> metaLevelOf: aClass [
	"Subclasses should decide what meta level of given class they see.
	Look at subclasses"
	self subclassResponsibility 
]

{ #category : #'meta level' }
ClyMetaLevelClassScope class >> metaLevelsOf: aClass do: aBlock [
	aBlock value: (self metaLevelOf: aClass)
]

{ #category : #'meta level' }
ClyMetaLevelClassScope >> metaLevelOf: aClass [
	^self class metaLevelOf: aClass
]
