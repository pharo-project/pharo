"
SUnit tests for namespaces
"
Class {
	#name : #RGNamespaceTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Containers-Base'
}

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
	self assert: (newNamespace packageNamed: #'Collections-Sequenceable') identicalTo: newPackage.
	self assert: (newNamespace classNamed: #OrderedCollection) identicalTo: newClass.
	self assertEmpty: newNamespace methods.
	self assertEmpty: newNamespace pools.
	self assertEmpty: newNamespace globalVariables.

	newNamespace removeClass: newClass.
	self assert: (newNamespace classNamed: #OrderedCollection) isNil.
	self assert: (newPackage classNamed: #OrderedCollection) notNil
]
