Class {
	#name : 'ObjectLayoutTest',
	#superclass : 'TestCase',
	#category : 'Kernel-Tests-Classes',
	#package : 'Kernel-Tests',
	#tag : 'Classes'
}

{ #category : 'tests' }
ObjectLayoutTest >> testLayoutForSubclassDefiningSymbol [

	self assert: (ObjectLayout layoutForSubclassDefiningSymbol: #variableWordSubclass:) equals: WordLayout.
	self assert: (ObjectLayout layoutForSubclassDefiningSymbol: #ephemeronSubclass:) equals: EphemeronLayout.
	self assert: (ObjectLayout layoutForSubclassDefiningSymbol: #weakSubclass:) equals: WeakLayout.
	self assert: (ObjectLayout layoutForSubclassDefiningSymbol: #variableSubclass: ) equals: VariableLayout.
	self assert: (ObjectLayout layoutForSubclassDefiningSymbol: #immediateSubclass: ) equals: ImmediateLayout.
	self assert: (ObjectLayout layoutForSubclassDefiningSymbol: #subclass: ) equals: FixedLayout.
	"here we can get back ByteLayout or CompiledMethodLayout, clients have to take that into account and check the class name"
	self assert: ({ByteLayout. CompiledMethodLayout} includes: (ObjectLayout layoutForSubclassDefiningSymbol: #variableByteSubclass:))
]
