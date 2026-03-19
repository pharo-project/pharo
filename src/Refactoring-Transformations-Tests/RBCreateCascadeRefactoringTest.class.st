Class {
	#name : 'RBCreateCascadeRefactoringTest',
	#superclass : 'RBAbstractTransformationTest',
	#instVars : [
		'class'
	],
	#category : 'Refactoring-Transformations-Tests-Test',
	#package : 'Refactoring-Transformations-Tests',
	#tag : 'Test'
}

{ #category : 'utilities' }
RBCreateCascadeRefactoringTest >> createCascadeFrom: sourceToCombine in: source [

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
RBCreateCascadeRefactoringTest >> setUp [
    super setUp.
    model := RBNamespace new.
    model defineClass: [ :aBuilder |
        aBuilder
            superclass: Object;
            name: #TestClass ].
    class := model classNamed: #TestClass.
]

{ #category : 'tests' }
RBCreateCascadeRefactoringTest >> testAssignmentOnlyLast [

| ref |
ref := self
    createCascadeFrom: 'obj foo.
    x := obj bar.'
    in: 'example
    obj foo.
    x := obj bar.'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example)
    equals:
    (self parseMethod: 'example
    x := obj
        foo;
        bar.')
]

{ #category : 'tests' }
RBCreateCascadeRefactoringTest >> testCascadeFlattening [

| ref |
ref := self
    createCascadeFrom: 'obj foo; bar.
    obj baz.'
    in: 'example
    obj foo; bar.
    obj baz.'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example)
    equals:
    (self parseMethod: 'example
    obj
        foo;
        bar;
        baz.')
]

{ #category : 'tests' }
RBCreateCascadeRefactoringTest >> testCreateCascadeBasic [

| ref |
ref := self
    createCascadeFrom: 'obj foo.
    obj bar.'
    in: 'example
    obj foo.
    obj bar.'.
ref generateChanges.
self assert:
    (class parseTreeForSelector: #example)
    equals:
    (self parseMethod: 'example
    obj
        foo;
        bar.')
]

{ #category : 'failure tests' }
RBCreateCascadeRefactoringTest >> testDifferentReceiversFail [

| ref |
ref := self
    createCascadeFrom: 'obj1 foo.
    obj2 bar.'
    in: 'example
    obj1 foo.
    obj2 bar.'.
self should: [ ref generateChanges ]
    raise: RBRefactoringError.
]

{ #category : 'tests' }
RBCreateCascadeRefactoringTest >> testMultipleAssignmentsFail [

| ref |
ref := self
    createCascadeFrom: 'x := obj foo.
    x := obj bar.'
    in: 'example
    x := obj foo.
    x := obj bar.'.
self should: [ ref generateChanges ]
    raise: RBRefactoringError.
]

{ #category : 'failure tests' }
RBCreateCascadeRefactoringTest >> testSingleStatementFails [

| ref |
ref := self
    createCascadeFrom: 'obj foo.'
    in: 'example
    obj foo.'.
self should: [ ref generateChanges ]
    raise: RBRefactoringError.
]
