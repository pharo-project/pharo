Class {
	#name : 'RBSplitCascadeRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
RBSplitCascadeRefactoringTest >> createCascadeFrom: sourceToCombine in: source [

| method start |

method := class compile: source classified: '#test data'.

start := method source findString: sourceToCombine.

^ RBCreateCascadeRefactoring
    model: model
    combine: (start to: (start + sourceToCombine size - 1))
    from: method selector
    in: class
]

{ #category : 'running' }
RBSplitCascadeRefactoringTest >> setUp [
    super setUp.
    model := RBNamespace new.
    model defineClass: [ :aBuilder |
        aBuilder
            superclass: Object;
            name: #TestClass ].
    class := model classNamed: #TestClass.
]

{ #category : 'utilities' }
RBSplitCascadeRefactoringTest >> splitCascadeFrom: sourceToSplit in: source [

| method start |

method := class compile: source classified: '#test data'.

start := method source findString: sourceToSplit.

^ RBSplitCascadeRefactoring
    model: model
    split: (start to: (start + sourceToSplit size - 1))
    from: method selector
    in: class
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testAssignmentSplit [

| ref |
ref := self
    splitCascadeFrom: ';'
    in: 'example
    x := obj foo;
        bar;
        baz'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example)
    equals:
    (self parseMethod: 'example
    obj foo.
    x := obj
        bar;
        baz.')
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testBasicSplit [

| ref |
ref := self
    splitCascadeFrom: ';'
    in: 'example
    obj foo;
        bar;
        baz'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example)
    equals:
    (self parseMethod: 'example
    obj foo.
    obj
        bar;
        baz.')
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testCreateThenSplitRoundTrip [

| createRef splitRef originalMethod |

"Step 1: original code"
originalMethod := self parseMethod: 'example
    obj foo.
    obj bar.'.
"Step 2: create cascade"
createRef := self
    createCascadeFrom: 'obj foo.
    obj bar.'
    in: 'example
    obj foo.
    obj bar.'.
createRef generateChanges.

"Step 3: split it back"
splitRef := self
    splitCascadeFrom: ';'
    in: ((class parseTreeForSelector: #example) source).
splitRef generateChanges.

"Step 4: compare ASTs"
self assert:
    (class parseTreeForSelector: #example)
    equals: originalMethod
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testExtractReceiver [

| ref |
ref := self
    splitCascadeFrom: ';'
    in: 'example
    (obj foo) bar;
        baz'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example) notNil.
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testInvalidSplitPosition [

| ref |
ref := self
    splitCascadeFrom: 'foo'
    in: 'example
    obj foo;
        bar;
        baz'.
self should: [ ref generateChanges ]
    raise: RBRefactoringError.
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testNotInCascade [

| ref |
ref := self
    splitCascadeFrom: 'foo'
    in: 'example
    obj foo.
    obj bar.'.
self should: [ ref generateChanges ]
    raise: RBRefactoringError.
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testReturnSplit [

| ref |
ref := self
    splitCascadeFrom: ';'
    in: 'example
    ^ obj foo;
        bar;
        baz'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example)
    equals:
    (self parseMethod: 'example
    obj foo.
    ^ obj
        bar;
        baz.')
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testSplitSingleMessage [

| ref |

ref := self
    splitCascadeFrom: ';'
    in: 'example
    obj foo;
        bar'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example)
    equals:
    (self parseMethod: 'example
    obj foo.
    obj bar.')
]

{ #category : 'tests' }
RBSplitCascadeRefactoringTest >> testWholeCascadeFail [

| ref |
ref := self
    splitCascadeFrom: 'foo; bar; baz'
    in: 'example
    obj foo;
        bar;
        baz'.
self should: [ ref generateChanges ]
    raise: RBRefactoringError.
]
