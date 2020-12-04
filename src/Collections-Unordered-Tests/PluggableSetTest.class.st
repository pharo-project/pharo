"
SUnit tests for pluggable sets
"
Class {
	#name : #PluggableSetTest,
	#superclass : #SetTest,
	#category : #'Collections-Unordered-Tests-Sets'
}

{ #category : #coverage }
PluggableSetTest class >> classToBeTested [

	^ IdentitySet
]

{ #category : #coverage }
PluggableSetTest >> classToBeTested [

	^ PluggableSet
]
