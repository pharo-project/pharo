"
I am a special kind of MethodAnnouncement which represent that overriding methods changes the ""override status"" because the method which they override (my affected method) is changed.

So I modify default MethodAnnouncement processing logic to affect overriding methods.
"
Class {
	#name : #ClyOverridingMethodsChanged,
	#superclass : #MethodAnnouncement,
	#instVars : [
		'announcerPlugin'
	],
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'instance creation' }
ClyOverridingMethodsChanged class >> initiatedBy: anInheritenceAnalysisPlugin forOverriddenMethod: aMethod [
	^self new 
		method: aMethod;
		announcerPlugin: anInheritenceAnalysisPlugin
]

{ #category : #testing }
ClyOverridingMethodsChanged >> affectsMethod: aMethod [
	^(aMethod selector = self overriddenMethod selector)
		and: [ aMethod origin inheritsFrom: self overriddenMethod origin]
]

{ #category : #testing }
ClyOverridingMethodsChanged >> affectsMethodsDefinedInClass: aClass [

	^ (aClass includesLocalSelector: self overriddenMethod selector)
			and: [ aClass inheritsFrom: self overriddenMethod origin ]
]

{ #category : #testing }
ClyOverridingMethodsChanged >> affectsMethodsDefinedInPackage: aPackage [

	^aPackage definesOverridesOf: self overriddenMethod
]

{ #category : #testing }
ClyOverridingMethodsChanged >> affectsMethodsTaggedWith: tagName [

	^announcerPlugin isMethodOverridden: method
]

{ #category : #accessing }
ClyOverridingMethodsChanged >> announcerPlugin [
	^ announcerPlugin
]

{ #category : #accessing }
ClyOverridingMethodsChanged >> announcerPlugin: anObject [
	announcerPlugin := anObject
]

{ #category : #accessing }
ClyOverridingMethodsChanged >> overriddenMethod [
	^method
]
