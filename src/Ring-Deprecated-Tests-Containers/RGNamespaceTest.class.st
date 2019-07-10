"
SUnit tests for namespaces
"
Class {
	#name : #RGNamespaceTest,
	#superclass : #TestCase,
	#category : #'Ring-Deprecated-Tests-Containers-Base'
}

{ #category : #deprecation }
RGNamespaceTest class >> isDeprecated [
	^true
]

{ #category : #testing }
RGNamespaceTest >> testCreatingNamespace [
	| newNamespace newPackage newClass |
	newNamespace := RGNamespace named: #RingNamespace.
	newPackage := RGPackageDefinition named: #'Collections-Sequenceable'.
	newClass := RGClassDefinition named: #OrderedCollection.
	newPackage addClass: newClass.
	newNamespace
		addPackage: newPackage;
		addClass: newClass.

	self assert: newNamespace isNamespace.
	self assert: (newNamespace packageNamed: #'Collections-Sequenceable') == newPackage.
	self assert: (newNamespace classNamed: #OrderedCollection) == newClass.
	self assertEmpty: newNamespace methods.
	self assertEmpty: newNamespace pools.
	self assertEmpty: newNamespace globalVariables.

	newNamespace removeClass: newClass.
	self assert: (newNamespace classNamed: #OrderedCollection) isNil.
	self assert: (newPackage classNamed: #OrderedCollection) notNil
]
