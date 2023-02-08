"
A `Generator` transforms callback interfaces into stream interfaces. 

The `Generator` class was originally written by Paolo Bonzini and inspired from functional programming languages.

With basic iterators, you specify an action to be perform to elements of a collection or stream but you do not control when computation pass from one element to the next one. In addition you cannot perform a lazzy computation on potentially infinite stream of objects. 

Generators offer a nice solution to such problem. Generators provide a way to use blocks to define a stream of values. The return values are computed one at a time, as needed, and hence need not even be finite. 

A generator needs a block and will make sure that the block is executed once at a time and that the block has the control to decide when to yield computation. A generator offers an API that is compatible with the one of stream since it act as a stream of objects. 

A generator should be instantiated using the expression `Generator on: [...]`. 
The following code creates an infinite streams of 1. 

```
| g | 
g := Generator on: [ :gen | [ gen yield: 1 ] repeat ].
g next
> 1
g next 
> 1
```

### Behavior 

The generator itself is passed to the block, and as soon as a message like `next`, `peek`, `atEnd` or `peekFor:` is sent to the generator, execution of the block starts/resumes and goes on until the generator's `yield:` method is called: then the argument of `yield:` will be the generator's next element. 

If the block goes on to the end without calling `yield:`, the generator will produce no more elements and `atEnd` will return true.


A generator is a quick way to create a stream of objects. A generator is a kind of pluggable stream, in that a user-supplied blocks defines which values are in a stream.

For example, here is an empty generator and two infinite generators:

```
Generator on: [ :gen | ]
Generator on: [ :gen | [ gen yield: 1 ] repeat ]
```

As a more concrete example taken from GNU Smalltalk implementation, these lines process a file and create Person objects out of the file:

```
lines := file lines.
lines := lines select: [ :line | line ~ '^[A-Za-z]+ [0-9]+$' ].
fields := lines collect: [ :line | line subStrings ].
people := fields collect: [ :data |
            Person name: data first age: data second asInteger ].
```
Let's see how to rewrite them to use a single Generator instead:

```
Generator on: [ :gen |
    file linesDo: [ :line || data |
        line ~ '^[A-Za-z]+ [0-9]+$' ifTrue: [
            data := line subStrings.
            gen yield: (Person name: data first age: data second asInteger) ] ] ].
```
As you can see, `select:` becomes an if-statement, and the value from the final stream is yielded to the user of the generator.
Generators use continuations, but they shield the users from their complexity by presenting the 
same simple interface as streams.


### Instance Variables

- block:		<BlockClosure> The block associated with the generator.
- continue:	<MethodContext>	The continuation to return to.
- home:		<MethodContext>	The home (root) context of the activated block
- next:		<Object>		The next object to return from the Generator.

"
Class {
	#name : #Generator,
	#superclass : #Stream,
	#instVars : [
		'block',
		'next',
		'continue',
		'home'
	],
	#category : #'Collections-Streams-Base'
}

{ #category : #'instance creation' }
Generator class >> on: aBlock [
	^ self basicNew initializeOn: aBlock
]

{ #category : #enumerating }
Generator class >> onDo: aBlock [

	"Helper to tranform any `do:`-like method into a generator."
	
	"|generator|
	generator := Generator onDo: [ :doBody | Integer primesUpTo: 10000000 do: doBody ].
	(generator next: 5) asArray >>> #(2 3 5 7 11)"
		
	^ self on: [ :generator | aBlock value: [ :arg | generator yield: arg ] ]
]

{ #category : #examples }
Generator class >> somePrimes [
	"self somePrimes"

	^ String streamContents: [ :str |
		| generator |
		generator := Generator on: [:g| Integer primesUpTo: 100 do:[:prime| g yield: prime]].
		[generator atEnd] whileFalse:[ str nextPutAll: generator next printString; cr]]
]

{ #category : #testing }
Generator >> atEnd [
	"Answer whether the receiver can access any more objects."

	^ continue isNil or: [ continue isDead ]
]

{ #category : #'open/close' }
Generator >> close [
	"Close the receiving generator and unwind its ensure-blocks."

	continue ifNotNil:[continue unwindTo: home].
	continue := block := next := nil
]

{ #category : #accessing }
Generator >> contents [
	"Answer the contents of this generator. Do not call this method on infinite generators."

	| stream |
	stream := (Array new: 10) writeStream.
	[ self atEnd ]
		whileFalse: [ stream nextPut: self next ].
	^ stream contents
]

{ #category : #private }
Generator >> fork [
	| result |
	home := thisContext.
	block reentrant value: self.
	thisContext swapSender: continue.
	result := next.
	continue := next := home := nil.
	^ result
]

{ #category : #initialization }
Generator >> initializeOn: aBlock [
	block := aBlock.
	self reset
]

{ #category : #accessing }
Generator >> next [
	"Generate and answer the next object in the receiver."

	^ self atEnd ifFalse: [
		home swapSender: thisContext sender.
		continue := thisContext swapSender: continue
	]
]

{ #category : #accessing }
Generator >> nextPut: anObject [
	"Add anObject into the generator. A synonym to #yield: and value:."

	| previous |
	previous := next.
	next := anObject.
	continue := thisContext swapSender: continue.
	^ previous
]

{ #category : #accessing }
Generator >> peek [
	"Answer the upcoming object of the receiver."

	^ next
]

{ #category : #printing }
Generator >> printOn: aStream [
	aStream nextPutAll: self class name; nextPutAll: ' on: '; print: block
]

{ #category : #initialization }
Generator >> reset [
	"Reset the generator, i.e., start it over"
	continue ifNotNil:[continue unwindTo: home].
	next := nil.
	continue := thisContext.
	self fork
]

{ #category : #accessing }
Generator >> size [
	"A generator does not know its size."

	^ self shouldNotImplement
]

{ #category : #public }
Generator >> value: anObject [
	"Allows passing generators as arguments to methods expecting blocks.
	A synonym for #yield: / #nextPut:."
	^ self nextPut: anObject
]

{ #category : #public }
Generator >> yield: anObject [
	"Yield the next value to the consumer of the generator.
	A synonym for #nextPut:"
	^ self nextPut: anObject
]
