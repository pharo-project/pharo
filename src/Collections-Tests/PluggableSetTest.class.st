"
SUnit tests for pluggable sets
"
Class {
	#name : #PluggableSetTest,
	#superclass : #SetTest,
	#category : #'Collections-Tests-Unordered'
}

{ #category : #'building suites' }
PluggableSetTest class >> classToBeTested [

^ IdentitySet
]

{ #category : #requirements }
PluggableSetTest >> classToBeTested [

	^ PluggableSet
]
