"
I report a class-side method with the pragma `<sampleInstance>` that does not return an instance of its class.

For example, this method is reported, because it answers a window:

```
MyPresenter class >> example
	<sampleInstance>
	^ self new open
```

The fix is to replace the pragma by `<script>`, which marks a method run for its effect:

```
MyPresenter class >> example
	<script>
	^ self new open
```

## Responsibility:

- Check the class-side methods having the pragma `<sampleInstance>`.
- Report those with no return of an instance of their class, as decided by my superclass.

## Collaborators:

- `ReAbstractSampleInstanceRule`: decides whether a method returns an instance of its class.
- `ReExampleShouldBeSampleInstanceRule`: the reverse rule, for example methods without the pragma.

### Implementation Points

- A method returning an instance through a factory not in the protocol `instance creation` is reported although it is right; `ReSampleInstanceMethodsTest` decides by running it.
"
Class {
	#name : 'ReSampleInstanceShouldReturnInstanceRule',
	#superclass : 'ReAbstractSampleInstanceRule',
	#category : 'General-Rules-Coding Idiom Violation',
	#package : 'General-Rules',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'accessing' }
ReSampleInstanceShouldReturnInstanceRule class >> rationale [

	^ 'A class-side method with the pragma <sampleInstance> should answer an instance of its class. A method that opens a window or answers another object, or nothing, is a demo: replace <sampleInstance> by <script>.'
]

{ #category : 'accessing' }
ReSampleInstanceShouldReturnInstanceRule class >> ruleName [

	^ 'A class-side method with the pragma <sampleInstance> should return an instance of its class'
]

{ #category : 'running' }
ReSampleInstanceShouldReturnInstanceRule >> basicCheck: aMethod [

	^ aMethod methodClass isClassSide and: [
		(aMethod hasPragmaNamed: #sampleInstance) and: [ (self returnsInstanceOfItsClass: aMethod) not ] ]
]
