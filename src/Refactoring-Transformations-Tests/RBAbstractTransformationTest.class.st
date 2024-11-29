Class {
	#name : 'RBAbstractTransformationTest',
	#superclass : 'RBAbstractEnvTestCase',
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'mocking' }
RBAbstractTransformationTest >> changeMockClass [

	^ RBRefactoringChangeMock
]

{ #category : 'mocking' }
RBAbstractTransformationTest >> objectClassVariable [

	^ Character classVariables first name
]

{ #category : 'parsing' }
RBAbstractTransformationTest >> parseMethod: aString [
	^ self parserClass parseMethod: aString
]

{ #category : 'parsing' }
RBAbstractTransformationTest >> parserClass [
	^ RBParser
]

{ #category : 'running' }
RBAbstractTransformationTest >> performAndUndoChange: aChange do: aBlock [
	"Perform a change in the system silently, evaluate aBlock and then undo the change again."

	^ self class codeChangeAnnouncer suspendAllWhile: [
		  | undo |
		  undo := aChange execute.
		  aBlock ensure: [ undo execute ] ]
]

{ #category : 'running' }
RBAbstractTransformationTest >> setUp [

	super setUp.
	model := RBNamespace new
]

{ #category : 'tests' }
RBAbstractTransformationTest >> testAPI [
	"all classes must implement #storeOn: and #privateTransform"

	| incompleteTransformations |
	incompleteTransformations := ReTransformation allSubclasses
		select: [ :each |
			| selectors |
			selectors := each methods collect: [:m | m selector].
			each subclasses isEmpty and: [ (selectors includes: #privateTransform) not and: [ (selectors includes: #storeOn:) not ] ] ].
	self assertEmpty: incompleteTransformations.

	incompleteTransformations := RBCompositeTransformation allSubclasses
		select: [ :each |
			| selectors |
			selectors := each methods collect: [:m | m selector].
			each subclasses isEmpty and: [ (selectors includes: #buildTransformations) not and: [ (selectors includes: #buildTransformationFor:) not ] ] ].
	self assertEmpty: incompleteTransformations
]

{ #category : 'tests' }
RBAbstractTransformationTest >> testBuildSelectorStringWithPermuteMapAndNewArguments [

	| selector permutations args trans selectorString |
	selector := #a123:b123:c123:.
	permutations := 1 to: 3.
	args := #().
	
	trans := ReAbstractTransformation new.
	selectorString := trans buildSelectorString: selector withPermuteMap: permutations andNewArguments: args.
	 
	self assert: selectorString equals: 'a123: ``@arg1 b123: ``@arg2 c123: ``@arg3 '
]

{ #category : 'tests' }
RBAbstractTransformationTest >> testBuildSelectorStringWithPermuteMapAndNewArgumentsWhenAllNewArguments [

	| selector permutations args trans selectorString |
	selector := #a123:c123:b123:.
	permutations := #(1 -2 -1).
	args := { RBArgumentName name: 'argN' value: '[ :each | each + 2 ]'.
	 			 RBArgumentName name: 'argM' value: '5'
			  }.
	
	trans := ReAbstractTransformation new.
	selectorString := trans buildSelectorString: selector withPermuteMap: permutations andNewArguments: args.
	 
	self assert: selectorString equals: 'a123: ``@arg1 c123: (5) b123: ([ :each | each + 2 ]) '
]

{ #category : 'tests' }
RBAbstractTransformationTest >> testBuildSelectorStringWithPermuteMapAndNewArgumentsWhenArgsPermuted [

	| selector permutations args trans selectorString |
	selector := #b123:a123:c123:.
	permutations := #(2 3 1).
	args := #().
	
	trans := ReAbstractTransformation new.
	selectorString := trans buildSelectorString: selector withPermuteMap: permutations andNewArguments: args.
	 
	self assert: selectorString equals: 'b123: ``@arg2 a123: ``@arg3 c123: ``@arg1 '
]

{ #category : 'tests' }
RBAbstractTransformationTest >> testBuildSelectorStringWithPermuteMapAndNewArgumentsWhenNewArgumentAdded [

	| selector permutations args trans selectorString |
	selector := #b123:a123:c123:.
	permutations := #(2 -1 1).
	args := { RBArgumentName name: 'anArg' value: 'OrderedCollection new' }.
	
	trans := ReAbstractTransformation new.
	selectorString := trans buildSelectorString: selector withPermuteMap: permutations andNewArguments: args.
	 
	self assert: selectorString equals: 'b123: ``@arg2 a123: (OrderedCollection new) c123: ``@arg1 '
]

{ #category : 'tests' }
RBAbstractTransformationTest >> testBuildSelectorStringWithPermuteMapAndNewArgumentsWhenNoArguments [

	| selector permutations args trans selectorString |
	selector := #b123.
	permutations := #(1).
	args := #().
	
	trans := ReAbstractTransformation new.
	selectorString := trans buildSelectorString: selector withPermuteMap: permutations andNewArguments: args.
	 
	self assert: selectorString equals: 'b123'
]
