"
I report a class-side method whose name starts with `example` and that returns an instance of its class, but has no pragma `<sampleInstance>`.

For example, this method is reported:

```
MyClass class >> exampleEmpty
	^ self new
		name: 'empty';
		yourself
```

The fix is to add the pragma: tools then know that the method answers a sample instance, and the smoke test `SmokeSampleInstanceMethodsTest`, subclassed by each project, checks it by running it.

```
MyClass class >> exampleEmpty
	<sampleInstance>
	^ self new
		name: 'empty';
		yourself
```

## Responsibility:

- Check the class-side methods whose selector starts with `example` and that have no pragma `<sampleInstance>`.
- Report those returning an instance of their class, as decided by my superclass.

## Collaborators:

- `ReAbstractSampleInstanceRule`: decides whether a method returns an instance of its class.
- `ReSampleInstanceShouldReturnInstanceRule`: the reverse rule, for methods having the pragma.
"
Class {
	#name : 'ReExampleShouldBeSampleInstanceRule',
	#superclass : 'ReAbstractSampleInstanceRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReExampleShouldBeSampleInstanceRule class >> rationale [

	^ 'A class-side method whose name starts with example and that returns an instance of its class is a sample instance: add the pragma <sampleInstance> so that tools find it and the smoke test SmokeSampleInstanceMethodsTest checks that it answers an instance of its class.'
]

{ #category : 'accessing' }
ReExampleShouldBeSampleInstanceRule class >> ruleName [

	^ 'A class-side example method returning an instance of its class should have the pragma <sampleInstance>'
]

{ #category : 'running' }
ReExampleShouldBeSampleInstanceRule >> basicCheck: aMethod [

	^ aMethod methodClass isClassSide and: [
		(aMethod selector beginsWith: 'example') and: [
			(aMethod hasPragmaNamed: #sampleInstance) not and: [ self returnsInstanceOfItsClass: aMethod ] ] ]
]
