"
I statically provide single ClyAllMethodGroup.

It is shown in the browser either with 'instance side' or 'class side' names depending on meta level of my method query
"
Class {
	#name : #ClyInheritedMethodGroupProvider,
	#superclass : #ClySingleMethodGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClyInheritedMethodGroupProvider >> createMethodGroupFor: aMethodQuery from: aClassScope [
	
	^ClyAllMethodGroup 
		withMethodQuery: aMethodQuery andClassesInheritedFrom: aClassScope
]

{ #category : #'building groups' }
ClyInheritedMethodGroupProvider >> createMethodQueryFrom: aClassScope [
	^ClyAllMethodsQuery from: aClassScope asLocalClassScope
]

{ #category : #testing }
ClyInheritedMethodGroupProvider >> isStatic [
	^true
]

{ #category : #testing }
ClyInheritedMethodGroupProvider >> shouldBuildGroupWith: aMethodQuery [
	
	^true
]
