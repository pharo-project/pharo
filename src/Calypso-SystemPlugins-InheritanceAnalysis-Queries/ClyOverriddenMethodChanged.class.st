"
I am a special kind of MethodAnnouncement which represent that overridden method changes own ""overridden status""
"
Class {
	#name : #ClyOverriddenMethodChanged,
	#superclass : #MethodAnnouncement,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #'instance creation' }
ClyOverriddenMethodChanged class >> method: aMethod [
	^self new 
		method: aMethod
]
