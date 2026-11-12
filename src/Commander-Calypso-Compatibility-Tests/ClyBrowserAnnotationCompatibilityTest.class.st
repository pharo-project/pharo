"
Tests that the deprecated class annotations of tabs, table decorators and selection contexts are still applied by Calypso while projects move to declarations with pragmas.
"
Class {
	#name : 'ClyBrowserAnnotationCompatibilityTest',
	#superclass : 'TestCase',
	#category : 'Commander-Calypso-Compatibility-Tests',
	#package : 'Commander-Calypso-Compatibility-Tests'
}

{ #category : 'tests' }
ClyBrowserAnnotationCompatibilityTest >> testSelectionContextAnnotationIsADeclaration [

	| declaration |
	declaration := ClySelectionContextDeclaration all detect: [ :each | each contextClass == ClyMockAnnotatedSelectionContext ].
	self assert: declaration browserClass identicalTo: ClyMockAnnotatedToolContext.
	self assert: declaration itemType identicalTo: ClyClass.
	self assert: declaration priority equals: 7.
	self assert: declaration declaringMethod identicalTo: ClyMockAnnotatedSelectionContext class >> #mockSelectionStrategy
]

{ #category : 'tests' }
ClyBrowserAnnotationCompatibilityTest >> testTabAnnotationIsATab [

	self assert: ((ClyCommandRegistry default activationsOf: #tabs inContext: ClyMockAnnotatedToolContext new) anySatisfy: [ :each | each toolClass == ClyMockAnnotatedTabTool ])
]

{ #category : 'tests' }
ClyBrowserAnnotationCompatibilityTest >> testTableDecorationAnnotationIsATableDecorator [

	| decorator |
	decorator := (ClyCommandRegistry default activationsOf: #tableDecorators inContext: ClyMockAnnotatedToolContext new) detect: [ :each | each annotatedClass == ClyMockAnnotatedTableDecorator ].
	self assert: decorator priority equals: 3
]
