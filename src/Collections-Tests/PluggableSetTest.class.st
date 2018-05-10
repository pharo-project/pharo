"
SUnit tests for pluggable sets
"
Class {
	#name : #PluggableSetTest,
	#superclass : #SetTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #coverage }
PluggableSetTest class >> classToBeTested [

	^ IdentitySet
]

{ #category : #coverage }
PluggableSetTest >> classToBeTested [

	^ PluggableSet
]
