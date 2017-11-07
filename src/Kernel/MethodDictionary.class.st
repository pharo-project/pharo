"
I'm a special dictionary holding methods. I am just like a normal Dictionary, except that I am implemented differently.  Each Class has an instance of MethodDictionary to hold the correspondence between selectors (names of methods) and methods themselves.

In a normal Dictionary, the instance variable 'array' holds an array of Associations.  Since there are thousands of methods in the system, these Associations waste space.  

Each MethodDictionary is a variable object, with the list of keys (selector Symbols) in the variable part of the instance.  The variable 'array' holds the values, which are CompiledMethods.

About flushCache methods and usage.

The VM caches method lookups in a lookup cache from class,selector pairs to method,primitive pairs, where primitive may be null.  This is called the first-level method lookup cache.  The JIT VM caches message lookups in machine code, so that a particular piece of machine code exists in a state that invokes a method for a specific class very fast by embedding the class reference in a register load and the target method in a call instruction, and having the target method verify this ""cache probe"" (this is an ""in-line cache).  The JIT also caches the translation of a byte coded method to machine code, by hiding a reference to a machine code method in a byte coded method.

These caches can be invalidated in several circumstances:

1a. if one adds or removes a method from a class's method dictionary it may change the correct results of a lookup not merely of the class whose dictionary was updated but also subclasses of the class.
1b. if one replaces a method in a method dictionary this changes the target method for a lookup of the selector for the class and subclasses

2. if one wants to rewrite the byte code or literals of a method, for example because a Slot definition has changed, then if the method has been compiled to machine code, the machine code must be discarded before the new code may be executed

1a & 1b are done via Symbol>>flushCache.  In response the normal VM flushes its first-level method lookup cache, and the JIT also scans all of machine code looking for inline caches with that selector, and voiding them, reverting each send site for that selector to the ""unlinked"" state.

There used to be confusion in Squeak, which Pharo inherited, that using CompiledMethod>>flushCache was somehow the right way to void caches when updating method dictionaries, flushing the old method in the dictionary, if any, and the new method.  It isn't, precisely because adding or removing methods affects the visibility of inherited methods with the same selector.  So MethodDictionary code should use Symbol>>flushCache, and only once, on each update of a method dictionary.  As a result, the VM will ensure that the necessary send caches are flushed for that selector.

2. is done via CompiledMethod>>flushCache.  In response the VM searches the first-level method lookup cache and removes all entries whose target is the method.  In addition the JIT discards the machine code for the method, and searches for all send sites with that method's machine code as the target and voids them, reverting them to the unlinked state.

The VM must be told to flush the cached state for a compiled method via CompiledMethod>>flushCache and will /try/ and void the state for that method.  But it can't always deal with existing activations of that method, because if there are activations running the machine code, that machine code can't merely be thrown away, and can't be replaced because its length may change, depending on literals or byte codes.  So this kind of byte coded method manipulation needs to be done with case and some understanding of the total system state.
"
Class {
	#name : #MethodDictionary,
	#superclass : #Dictionary,
	#type : #variable,
	#category : #Kernel-Methods
}

{ #category : #initialization }
MethodDictionary class >> compactAllInstances [

	| instancesToExchange newInstances |
	instancesToExchange := Array streamContents: [ :oldStream |
		newInstances := Array streamContents: [ :newStream |
			self allInstances do: [ :each |
				| newInstance |
				newInstance := each compactWithoutBecome.
				newInstance capacity = each capacity 
					ifTrue: [ each copyFrom: newInstance ]
					ifFalse: [
						oldStream nextPut: each.
						newStream nextPut: newInstance ] ] ] ].
	instancesToExchange elementsForwardIdentityTo: newInstances
]

{ #category : #'instance creation' }
MethodDictionary class >> new [
	"Create a new instance with 32 slots, which can hold at most 24 methods before growing is necessary."
	
	^self newForCapacity: 32
]

{ #category : #'instance creation' }
MethodDictionary class >> new: numberOfElements [
	"Create an instance large enough to hold numberOfElements methods without growing."
	
	^self newForCapacity: (self sizeFor: numberOfElements)
]

{ #category : #'instance creation' }
MethodDictionary class >> newForCapacity: capacity [
	"Create an instance with the given capacity which must be a power of two."
	
	^(self basicNew: capacity) initialize: capacity
]

{ #category : #sizing }
MethodDictionary class >> sizeFor: numberOfElements [
    "Return the minimum capacity of a dictionary that can hold numberOfElements elements. At least 25% of the array must be empty and the return value must be a nonnegative power of 2. Notice that the max: 1 is because a MethodDictionaries can never be entirely empty, as the #grow method requires it not to be (since it does self basicSize * 2)"

	^(numberOfElements * 4 // 3 max: 1) asLargerPowerOfTwo
]

{ #category : #accessing }
MethodDictionary >> add: anAssociation [

	^ self at: anAssociation key put: anAssociation value
]

{ #category : #accessing }
MethodDictionary >> associationAt: key ifAbsent: aBlock [
      "Answer the association with the given key.
       If key is not found, return the result of evaluating aBlock."

       ^(array at: (self scanFor: key))
               ifNil: [ aBlock value ]
               ifNotNil: [ :value | key -> value ]
]

{ #category : #enumeration }
MethodDictionary >> associationsDo: aBlock [ 

	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil:
			[ :key | aBlock value: (Association key: key value: (array at: i))]]
]

{ #category : #accessing }
MethodDictionary >> at: key ifAbsent: aBlock [

	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	^ array at: index
]

{ #category : #accessing }
MethodDictionary >> at: key ifPresent: aBlock [

	^(array at: (self findElementOrNil: key))
		ifNotNil: [ :value | aBlock cull: value ]
]

{ #category : #accessing }
MethodDictionary >> at: key put: value [
	"Set the value at key to be value."
	| index |
	index := self findElementOrNil: key.
	(self basicAt: index) 
		ifNil: 
			[tally := tally + 1.
			self basicAt: index put: key].
	array at: index put: value.
	key flushCache. "flush the vm cache by selector"
	self fullCheck.
	^ value
]

{ #category : #private }
MethodDictionary >> compact [
	"Make sure that I have the highest possible load factor (between 37.5% and 75%)."
	
	| newInstance |
	newInstance := self compactWithoutBecome.
	newInstance capacity = self capacity
		ifTrue: [ self copyFrom: newInstance ]
		ifFalse: [ self becomeForward: newInstance ]
]

{ #category : #private }
MethodDictionary >> compactWithoutBecome [
	"Return a copy of self which has the highest possible load factor (between 37.5% and 75%)."
	
	| newInstance |
	newInstance := self species new: self size.
	1 to: self basicSize do: [ :index | 
		(self basicAt: index) ifNotNil: [ :key |
			newInstance at: key put: (array at: index) ] ].
	^newInstance
]

{ #category : #private }
MethodDictionary >> fixCollisionsFrom: start [
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."

	| key index |
	index := start.
	[ (key := self basicAt: (index := index \\ array size + 1)) == nil ] whileFalse: [
		| newIndex |
		(newIndex := self findElementOrNil: key) = index ifFalse: [
			self swap: index with: newIndex ] ]
]

{ #category : #private }
MethodDictionary >> grow [ 
	| newSelf |
	newSelf := self species newForCapacity: self basicSize * 2.
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil: [ :key | newSelf at: key put: (array at: i)]].
	self becomeForward: newSelf
]

{ #category : #testing }
MethodDictionary >> isHealthy [
	"Test that selector hashes match their positions stored in dictionary,
	answer true if everything ok, false otherwise
	
	MethodDictionary allInstances select: [:dict |
		dict isHealthy not ]

	"
	1 to: self basicSize do: [:i | | selector |
		selector := self basicAt: i.
		selector ifNotNil: [
			(self scanFor: selector) == i ifFalse: [ ^ false ]]].
	^ true
]

{ #category : #accessing }
MethodDictionary >> keyAtIdentityValue: value ifAbsent: exceptionBlock [
	"Answer the key whose value equals the argument, value. If there is
	none, answer the result of evaluating exceptionBlock."
	1 to: self basicSize do:
		[:index |
		value == (array at: index)
			ifTrue:
				[(self basicAt: index) ifNotNil: [ :theKey | ^ theKey]]].
	^ exceptionBlock value
]

{ #category : #accessing }
MethodDictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key whose value equals the argument, value. If there is
	none, answer the result of evaluating exceptionBlock."
	1 to: self basicSize do:
		[:index |
		value = (array at: index)
			ifTrue:
				[(self basicAt: index) ifNotNil: [ :theKey | ^ theKey]]].
	^ exceptionBlock value
]

{ #category : #enumeration }
MethodDictionary >> keysAndValuesDo: aBlock [ 
	"Enumerate the receiver with all the keys and values passed to the block"
	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil:
			[ :key | aBlock value: key value: (array at: i)]
		]
]

{ #category : #enumeration }
MethodDictionary >> keysDo: aBlock [ 
	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (self basicAt: i) ifNotNil: [ :key | aBlock value: key]]
]

{ #category : #copying }
MethodDictionary >> postCopy [
	array := array copy
]

{ #category : #private }
MethodDictionary >> rehash [ 

	| newInstance |
	newInstance := self species newForCapacity: self basicSize.
	1 to: self basicSize do: [ :index | 
		(self basicAt: index) ifNotNil: [ :key |
			newInstance at: key put: (array at: index) ] ].
	self copyFrom: newInstance
]

{ #category : #removing }
MethodDictionary >> removeAll [
	"Remove all elements from this collection. Preserve the capacity"
	
	| newSelf |
	tally = 0 ifTrue: [^self].
	newSelf := self species newForCapacity: self basicSize.
	self copyFrom: newSelf
]

{ #category : #private }
MethodDictionary >> removeDangerouslyKey: key ifAbsent: aBlock [
	"This is not really dangerous.  But if normal removal
	were done WHILE a MethodDict were being used, the
	system might crash.  So instead we make a copy, then do
	this operation (which is NOT dangerous in a copy that is
	not being used), and then use the copy after the removal."

	| index element |
	index := self findElementOrNil: key.
	(self basicAt: index) ifNil: [ ^ aBlock value ].
	element := array at: index.
	array at: index put: nil.
	self basicAt: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ element
]

{ #category : #removing }
MethodDictionary >> removeKey: key ifAbsent: errorBlock [ 
	"The interpreter might be using this MethodDictionary while
	this method is running! Therefore we perform the removal
	in a copy, and then atomically copy that copy"
	
	| copy removedValue |
	copy := self copy.
	removedValue := copy removeDangerouslyKey: key ifAbsent: [^ errorBlock value].
	self copyFrom: copy.
	key flushCache.
	^ removedValue
]

{ #category : #private }
MethodDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject basicIdentityHash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := self basicAt: index) isNil or: [element == anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := self basicAt: index) isNil or: [element == anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : #private }
MethodDictionary >> swap: oneIndex with: otherIndex [

	| element |
	element := self basicAt: oneIndex.
	self basicAt: oneIndex put: (self basicAt: otherIndex).
	self basicAt: otherIndex put: element.
	array swap: oneIndex with: otherIndex
]

{ #category : #enumeration }
MethodDictionary >> valuesDo: aBlock [ 
	tally = 0 ifTrue: [^ self].
	1 to: self basicSize do:
		[:i | (array at: i) ifNotNil: [ :value | aBlock value: value]]
]
