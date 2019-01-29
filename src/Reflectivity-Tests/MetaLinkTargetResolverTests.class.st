"
Note that for API with options the same methods are called for objects and classes, so the test is made only once.
"
Class {
	#name : #MetaLinkTargetResolverTests,
	#superclass : #TestCase,
	#category : #'Reflectivity-Tests'
}

{ #category : #tests }
MetaLinkTargetResolverTests >> testFindClassVariable [
	| classVar |
	classVar := ReflectivityExamples classVariableNamed: 'singleton'.
	self assert: (MetalinkTargetResolver classVariableNamed: 'singleton' for: ReflectivityExamples) == classVar.
	self assert: (MetalinkTargetResolver classVariableNamed: 'singleton' for: ReflectivityExamples new) == classVar
]

{ #category : #tests }
MetaLinkTargetResolverTests >> testFindClassVariableWithOptions [
	| classVar readNodes writeNodes |
	classVar := ReflectivityExamples classVariableNamed: 'singleton'.
	readNodes := classVar readNodes asIdentitySet.
	writeNodes := classVar assignmentNodes asIdentitySet.
	self assert: (MetalinkTargetResolver classVariableNamed: 'singleton' for: ReflectivityExamples option: #read) = readNodes.
	self assert: (MetalinkTargetResolver classVariableNamed: 'singleton' for: ReflectivityExamples option: #write) = writeNodes.
	self
		assert:
			(MetalinkTargetResolver classVariableNamed: 'singleton' for: ReflectivityExamples option: #all)
				= (Array with: writeNodes with: readNodes) flattened asIdentitySet
]

{ #category : #tests }
MetaLinkTargetResolverTests >> testFindMethod [
	| ast  |
	ast := (ReflectivityExamples >> #exampleMethod) ast.
	self assert: (MetalinkTargetResolver methodNamed: #exampleMethod for: ReflectivityExamples) == ast.
	self assert: (MetalinkTargetResolver methodNamed: #exampleMethod for: ReflectivityExamples new) == ast
]

{ #category : #tests }
MetaLinkTargetResolverTests >> testFindSlot [
	| slot |
	slot := ReflectivityExamples slotNamed: 'tag'.
	self assert: (MetalinkTargetResolver slotNamed: 'tag' for: ReflectivityExamples) == slot.
	self assert: (MetalinkTargetResolver slotNamed: 'tag' for: ReflectivityExamples new) == slot
]

{ #category : #tests }
MetaLinkTargetResolverTests >> testFindSlotWithOptions [
	| slot readNodes writeNodes |
	slot := ReflectivityExamples slotNamed: 'tag'.
	readNodes := slot readNodes asIdentitySet.
	writeNodes := slot assignmentNodes asIdentitySet.
	self assert: (MetalinkTargetResolver slotNamed: 'tag' for: ReflectivityExamples option: #read) = readNodes.
	self assert: (MetalinkTargetResolver slotNamed: 'tag' for: ReflectivityExamples option: #write) = writeNodes.
	self
		assert:
			(MetalinkTargetResolver slotNamed: 'tag' for: ReflectivityExamples option: #all)
				= (Array with: writeNodes with: readNodes) flattened asIdentitySet
]

{ #category : #tests }
MetaLinkTargetResolverTests >> testFindTemporaryVariable [
	| temp |
	temp := (ReflectivityExamples >> #exampleIfTrueIfFalse) temporaryVariableNamed: #t.	
	self assert: (MetalinkTargetResolver temporaryNamed: #t inMethod: #exampleIfTrueIfFalse for: ReflectivityExamples) = temp.
	self assert: (MetalinkTargetResolver temporaryNamed: #t inMethod: #exampleIfTrueIfFalse for: ReflectivityExamples new) = temp.
]

{ #category : #tests }
MetaLinkTargetResolverTests >> testFindTemporaryVariableWithOptions [
	| temp readNodes writeNodes |
	temp := ReflectivityExamples >> #exampleIfTrueIfFalse temporaryVariableNamed: #t.
	readNodes := temp readNodes asIdentitySet.
	writeNodes := temp assignmentNodes asIdentitySet.
	self
		assert:
			(MetalinkTargetResolver
				temporaryNamed: #t
				inMethod: #exampleIfTrueIfFalse
				for: ReflectivityExamples
				option: #read) = readNodes.
	self
		assert:
			(MetalinkTargetResolver
				temporaryNamed: #t
				inMethod: #exampleIfTrueIfFalse
				for: ReflectivityExamples
				option: #write) = writeNodes.
	self
		assert:
			(MetalinkTargetResolver
				temporaryNamed: #t
				inMethod: #exampleIfTrueIfFalse
				for: ReflectivityExamples
				option: #all) = (Array with: writeNodes with: readNodes) flattened asIdentitySet
]

{ #category : #tests }
MetaLinkTargetResolverTests >> testRecoverClassFromEntity [
	self assert: (MetalinkTargetResolver classFor: ReflectivityExamples new) == ReflectivityExamples.
	self assert: (MetalinkTargetResolver classFor: ReflectivityExamples) == ReflectivityExamples
]
