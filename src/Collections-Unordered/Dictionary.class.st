"
I am a collection of elements that associate a key object with a value object.

## Description


I  can be viewed from one of two perspectives: a set of associations, or a container of values that are externally named where the name can be any object that responds to #=. The external name is referred to as the key.  I inherit many operations from Set.
I use the #= method to locate a key. If you want a collection that use the #== method (same pointers) you can use an IdentityDictionary.

I'm used when we need a collection of objects which I can access with a key. For example if you associate some words with a definition, the word will be the key and the definition will be the value. Both of them can be any kind of objects.

Internally I use Associations. The Association class can help to use me. (See examples lower)

I assume that my keys do not change after they have been added. The behavior of operations involving keys that have changed is undefined.

## Public API and Key Messages

### Initializing and Adding

A `Dictionary` can be created with values using the `Collection class>>#withAll:` class selector which will add the 
values of a collection as the values of the `Dictionary` using numeric keys.

```
d := Dictionary withAll: #(1 2 3 4 5)  ""a Dictionary(1->1, 2->2, 3->3, 4->4, 5->5)""
```

Alternatively, both the keys and values can be supplied using the `Dictionary class>>#newFromKeys:andValues:` selector,
which requires two collections of the same length, or the `Dictionary class>>#newFromPairs:` selector, which 
uses every odd numbered index as a key and even numbered index as a value for the dictionary.

Once a `Dictionary` has been created, the basic method for adding new keys is the `Dictionary>>#at:put:` selector.

```
d at: 99 put: 89  ""a Dictionary(1->1, 2->2, 3->3, 4->4, 5->5, 99->89)""
```

### Accessing 

Get the value of a key using the `Dictionary>>#at:` selector.

```
d := Dictionary withAll: #(5 4 3 2 1)  ""a Dictionary(1->5, 2->4, 3->3, 4->2, 5->1)""
d at: 1  ""5""
```
However accessing a key that does not exist will result in an error. In which case use
the selectors `Dictionary>>#at:ifAbsent:`

```
d at: 100 ifAbsent: [ ""execute code in the block if the key is not found"" ]
```

A common operation is to add in a value for a key if it is absent using the `Dictionary>>#at:ifAbsentPut:` selector. Note that unlike the regular `Dictionary>>#at:put:` selector, this message uses the value of a block. 

```
d at: 100 ifAbsentPut: [ 100 ]
```

Alternatively, if you want to change the value of a key use `Dictionary>>#at:update:` selector,
which uses the value of a block.

```
d at: 5 update: [ 12 ]
""a Dictionary(1->5,2->4,3->3,4->2,5->12,100->100)""
```

### Iterating / Enumerating

It's possible to iterate over all of the values, keys, and associations of a `Dictionary` using 
the `Dictionary>>#valuesDo:`, `Dictionary>>#keysDo:`, and `Dictionary>>#associationsDo:` selectors. These selectors evaluate a block for
each of the items

```
""do: is an alias for valuesDo:""
d valuesDo: [ :eachValue | ""do something with the value"" ].
d keysDo: [ :eachKey | ""do something with the key"" ].
d associationsDo: [ :eachAssociation | ""do something with a key-value pair"" ]
```

The `Dictionary>>#select:` selector is also implemented to return a subset of the `Dictionary` where the
block evaluates to true using the value.

```
d select: [:each | each > 5]  ""a Dictionary(5->12,100->100)""
``` 
		
### Removing

Use the `Dictionary>>#removeKey:` selector to remove the association from the `Dictionary`, which will cause
an error if the key is not found. Use the `Dictionary>>#removeKey:ifAbsent:` selector to control this behavior.

```
d removeKey: 5.
d removeKey: 1200.  ""Error""
d removeKey: 1200 ifAbsent: [ ""do something"" ].
```

### Testing

Check if a value or key is present in a `Dictionary` using the `Dictionary>>#includes:` or `Dictionary>>#includesKey:` selectors.

```
""Look if 12 is the value of any key in the Dictionary""
d includes: 12.

""Look if 100 is a key in the Dictionary""
d includesKey: 100.
```
## Examples 


To create a dictionary with indexes as key: 

```
	Dictionary withAll: #(7 3 1 3)   		""returns:  a Dictionary(1->7 2->3 3->1 4->3 ""
```

To use Objects as key (here symbols): 

```
	colors := Dictionary new 
				at: #yellow put: Color yellow; 
				at: #blue put: Color blue;
				at: #red put: Color red;
				yourself.
				
	colors at: #yellow. 	""returns:  Color yellow""
	colors keys          ""returns: a Set(#blue #yellow #red)""
	colors values       ""returns:  {Color blue. Color yellow. Color red}"" 
```

You can also directly use Associations: 

```
	colors := Dictionary with: #yellow -> Color yellow with: #blue -> Color blue.
	colors add: #red -> Color red.
	
	colors associations  	""returns: {#yellow->Color yellow. #red->Color red. #blue->Color blue}""
```
	
Here some more examples: 

```
	colors := Dictionary newFrom: { #blue->Color blue . #red->Color red . #yellow->Color yellow }. 
	colors removeKey: #blue. 
	colors at: #red ifPresent: [ :color |  color darker] ifAbsent: [ Error signal: 'The red color should be here.' ] .		""return: (Color r: 0.92 g: 0.0 b: 0.0 alpha: 1.0)""
	colors associations 		 ""{#yellow->Color yellow. #red->Color red}""
```

## Internal Representation and Key Implementation Points.

I am just a collection of associations. If I need my keys I will just return the keys of my associations. Idem for my values.
I use the #= method in order to manipulate my keys. I cannot have two associations that are equals with the #= method.
"
Class {
	#name : #Dictionary,
	#superclass : #HashedCollection,
	#category : #'Collections-Unordered-Dictionaries'
}

{ #category : #'instance creation' }
Dictionary class >> newFrom: aDictionaryOrCollectionOfAssociations [
	"Answer an instance of me containing the same associations as the argument.
	If the same key appears twice, the last one enumerated will win"

	"(Dictionary newFrom: {1->#a. 2->#b. 3->#c}) >>> ({1->#a. 2->#b. 3->#c} asDictionary)"

	| newDictionary |
	newDictionary := self new: aDictionaryOrCollectionOfAssociations size.
	aDictionaryOrCollectionOfAssociations associationsDo: [:x |newDictionary add: x].
	^ newDictionary
]

{ #category : #'instance creation' }
Dictionary class >> newFromKeys: keys andValues: values [
	"Create a dictionary from the keys and values arguments which should have the same length."
	"(Dictionary newFromKeys: #(#x #y) andValues: #(3 6)) >>> (Dictionary new at: #x put: 3; at: #y put: 6 ;yourself)"

	| dict |
	dict := self new.
	keys with: values do: [ :k :v | dict at: k put: v ].
	^ dict
]

{ #category : #'instance creation' }
Dictionary class >> newFromPairs: anArray [
	"Answer an instance of me associating (anArray at: i) to (anArray at: i+1)
	 for each odd i.  anArray must have an even number of entries."

	"Dictionary newFromPairs: {'Red' . Color red . 'Blue' . Color blue . 'Green' . Color green}."

	| newDictionary |
	newDictionary := self new: anArray size / 2.
	1 to: anArray size - 1 by: 2 do: [ :i | newDictionary at: (anArray at: i) put: (anArray at: i + 1) ].
	^ newDictionary
]

{ #category : #comparing }
Dictionary >> = aDictionary [
	"Two dictionaries are equal if
	 (a) they are the same 'kind' of thing.
	 (b) they have the same set of keys.
	 (c) for each (common) key, they have the same value.
	See issue 16760 before changing"

	self == aDictionary ifTrue: [^true].
	self species == aDictionary species ifFalse: [^false].
	self size = aDictionary size ifFalse: [^false].
	self associationsDo: [:assoc|
		(aDictionary at: assoc key ifAbsent: [^false]) = assoc value
			ifFalse: [^false]].
	^true
]

{ #category : #adding }
Dictionary >> add: anAssociation [

	"Add anAssociation to the dictionary. If the key is already in the dictionary then the value
	will overwrite the one currently present.

	```
	d := Dictionary new at: 5 put: 1; yourself.  ""a Dictionary(5->1)""
	d add: 5-> 12.  ""a Dictionary(5->12)""
	a add: 4->4. ""a Dictionary(5->12,4->4)""
	```
	"

	| index element |
	index := self findElementOrNil: anAssociation key.
	element := array at: index.
	element
		ifNil: [ self atNewIndex: index put: anAssociation ]
		ifNotNil: [ element value: anAssociation value ].
	^ anAssociation
]

{ #category : #adding }
Dictionary >> addAll: aKeyedCollection [

	"Add all of the keys and values from aKeyedCollection to the dictionary.
	If the key already exists then the value is overwritten.

	```
	d1 := Dictionary new at: 1 put: 2; at: 2 put: 5; yourself.
	d2 := Dictionary new at: 1 put: 4; at: 100 put 6; yourself.
	d1 addAll: d2.  ""d1 now a Dictionary(1->4,2->5,100->6)""
	```
	"

	aKeyedCollection == self ifFalse: [
		aKeyedCollection keysAndValuesDo: [ :key :value |
			self at: key put: value ] ].
	^ aKeyedCollection
]

{ #category : #accessing }
Dictionary >> associationAt: key [
	"Returns the association for the given key."

	^ self associationAt: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #accessing }
Dictionary >> associationAt: key ifAbsent: aBlock [
	"Answer the association with the given key.
	If the key is not found, return the result of evaluating aBlock."

	^ (array at: (self findElementOrNil: key))
		ifNil: [ aBlock value ]
		ifNotNil: [ :assoc | assoc ]
]

{ #category : #accessing }
Dictionary >> associationAt: key ifPresent: aBlock [
	"Answer the value of evaluating aBlock optionally with the association
	for the given key. If the key is not found, return nil."

	^ (array at: (self findElementOrNil: key))
		ifNotNil: [ :assoc | aBlock cull: assoc ]
]

{ #category : #accessing }
Dictionary >> associationAt: key ifPresent: aPresentBlock ifAbsent: anAbsentBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the association for the key.
	Otherwise answer the value of the second block."

	self associationAt: key ifPresent: [:assoc | ^ aPresentBlock cull: assoc].
	^ anAbsentBlock value
]

{ #category : #accessing }
Dictionary >> associations [
	"Answer a collection containing the receiver's associations."
	"Suggested by l. Uzonyi"

	^Array new: self size streamContents: [ :stream |
		self associationsDo: [ :each | stream nextPut: each ] ]
]

{ #category : #enumerating }
Dictionary >> associationsDo: aBlock [

	"Evaluate aBlock for each of the receiver's elements (key/value
	associations). See keysDo: or valuesDo: if you only need to evaluate
	one or the other. Also see keysAndValuesDo: for a similar
	selector where the block accepts a two agruments.

	```
	d := Dictionary withAll: #(4 5 9 6 76).
	a := OrderedCollection new.
	d associationsDo: [ :assoc | a add: assoc value. a add: assoc key ].
	a. ""an OrderedCollection(76 5 9 3 4 1 6 4 5 2)""
	```
	"

	tally = 0 ifTrue: [ ^ self ].
	array do: [ :each | each ifNotNil: [ aBlock value: each ] ]
]

{ #category : #enumerating }
Dictionary >> associationsSelect: aBlock [
	"Evaluate aBlock with each of my associations as the argument. Collect
	into a new dictionary, only those associations for which aBlock evaluates
	to true."

	| newCollection |
	newCollection := self species new.
	self associationsDo:
		[:each |
		(aBlock value: each) ifTrue: [newCollection add: each]].
	^newCollection
]

{ #category : #accessing }
Dictionary >> at: key [
	"Answer the value associated with the key."
	^ self at: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #'nested dictionaries' }
Dictionary >> at: firstKey at: secondKey [
	"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey."

	"
	(Dictionary new
		at: #top at: #below1 put: 1;
		at: #top at: #below1 put: 2;
		at: #top at: #below1)
	>>>
	2"

	^ self at: firstKey at: secondKey ifAbsent: [self errorKeyNotFound: secondKey]
]

{ #category : #'nested dictionaries' }
Dictionary >> at: firstKey at: secondKey ifAbsent: aZeroArgBlock [
		"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey. Execute aZeroArgBlock in case one of the key is wrong."

	| subDictionary |
	subDictionary := self at: firstKey ifAbsent: [ ^ aZeroArgBlock value ].
	^ subDictionary at: secondKey ifAbsent: aZeroArgBlock
]

{ #category : #'nested dictionaries' }
Dictionary >> at: firstKey at: secondKey ifAbsentPut: aZeroArgBlock [
	"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey. If firstKey is not defined, set a new dictionary for the second key and set the value of aZeroArgBlock execution. If firstKey is defined and not second key set the value of aZeroArgBlock execution. See NestedDictionaryTest for examples."

	| subDictionary |
	subDictionary := self at: firstKey ifAbsentPut: [ self species new ].
	^ subDictionary at: secondKey ifAbsentPut: aZeroArgBlock
]

{ #category : #'nested dictionaries' }
Dictionary >> at: firstKey at: secondKey put: aValue [
	"Set a value at secondKey in the dictionary returned by firstKey."

	| subDictionary |
	subDictionary := self at: firstKey ifAbsentPut: [ self species new ].
	^ subDictionary at: secondKey put: aValue
]

{ #category : #accessing }
Dictionary >> at: key ifAbsent: aBlock [
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	^((array at: (self findElementOrNil: key))
			ifNil: [aBlock]
			ifNotNil: [:assoc | assoc]) value
]

{ #category : #accessing }
Dictionary >> at: key ifAbsentPut: aBlock [
	"Return the value at the given key.
	If the key is not included in the receiver store and return the result
	of evaluating aBlock as the new value."

	^ self at: key ifAbsent: [self at: key put: aBlock value]
]

{ #category : #accessing }
Dictionary >> at: key ifPresent: aBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the given block optionally with the value associated
	with the key.
	Otherwise, answer nil."

	^(array at: (self findElementOrNil: key))
		ifNotNil: [:assoc | aBlock cull: assoc value]
]

{ #category : #accessing }
Dictionary >> at: key ifPresent: aPresentBlock ifAbsent: anAbsentBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the value associated
	with the key.
	Otherwise answer the value of the second block."

	self at: key ifPresent: [:v | ^ aPresentBlock cull: v].
	^ anAbsentBlock value
]

{ #category : #accessing }
Dictionary >> at: key ifPresent: aPresentBlock ifAbsentPut: anAbsentBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the value associated
	with the key.
	Otherwise store and return the result of evaluating the second block as the
	new value of the key."

	^ self
		at: key
		ifPresent: aPresentBlock
		ifAbsent: [self at: key put: anAbsentBlock value]
]

{ #category : #accessing }
Dictionary >> at: key put: anObject [
	"Set the value at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc
		ifNil: [self atNewIndex: index put: (Association key: key value: anObject)]
		ifNotNil: [assoc value: anObject].
	^ anObject
]

{ #category : #accessing }
Dictionary >> at: key update: updateBlock [
	"I am used to update the value at a given key, or if the key does not exist, to throw an error"
	self at: key update: updateBlock initial: [ self errorKeyNotFound: key ]
]

{ #category : #accessing }
Dictionary >> at: key update: updateBlock initial: initBlocktOrValue [
	"I am used to update the value at a given key. The updateBlock is passed
	the existing value, and the result of the block is stored back.
	If the key does not exist, store the value of the initBlocktOrValue.
	initBlocktOrValue can be a block in case the initial value is expencive to compute.
	I use findElementOrNil: to avoid looking up the key twice."
	| index |
	index := self findElementOrNil: key.
	(array at: index)
		ifNil: [ self atNewIndex: index put: key -> initBlocktOrValue value]
		ifNotNil: [ :assoc | assoc value: (updateBlock value: assoc value) ]
]

{ #category : #accessing }
Dictionary >> bindingOf: varName [
	^self associationAt: varName ifAbsent:[nil]
]

{ #category : #enumerating }
Dictionary >> bindingsDo: aBlock [

	"An alias for associationsDo:"

	self associationsDo: aBlock
]

{ #category : #enumerating }
Dictionary >> collect: aBlock [
	"Evaluate aBlock with each of my values as the argument.  Collect the
	resulting values into a collection that is like me. Answer with the new
	collection."
	| newCollection |
	newCollection := self species new.
	self associationsDo:[:each |
		newCollection at: each key put: (aBlock value: each value).
	].
	^newCollection
]

{ #category : #adding }
Dictionary >> declare: key from: aDictionary [
	"Add key to the receiver. If key already exists, do nothing. If aDictionary
	includes key, then remove it from aDictionary and use its association as
	the element of the receiver."

	(self includesKey: key) ifTrue: [^ self].
	(aDictionary includesKey: key)
		ifTrue:
			[self add: (aDictionary associationAt: key).
			aDictionary removeKey: key]
		ifFalse:
			[self add: key -> nil]
]

{ #category : #enumerating }
Dictionary >> difference: aCollection [
	"Answer the set theoretic difference of two collections. This is a specialized version for Dictionaries keeping the keys of the objects. At a slightly higher price of an additional Set to track duplicates."

	| other result duplicates |

	other := aCollection asSet.
	duplicates := Set new.
	result := self class new: self size.

	self keysAndValuesDo: [ :key :value|
		((other includes: value) not and: [ (duplicates includes: value) not ])
			ifTrue: [
				duplicates add: value.
				result at: key put: value]].

	^ result
]

{ #category : #enumerating }
Dictionary >> do: aBlock [

	"An alias for valuesDo:. Evaluate aBlock for each of my values."

	^ self valuesDo: aBlock
]

{ #category : #private }
Dictionary >> errorKeyNotFound: aKey [

	KeyNotFound signalFor: aKey
]

{ #category : #private }
Dictionary >> errorValueNotFound: value [

	ValueNotFound signalFor: value
]

{ #category : #private }
Dictionary >> fixCollisionsFrom: start [
	"The element at start has been removed and replaced by nil.
	This method moves forward from there, relocating any entries
	that had been placed below due to collisions with this one."
	| element index |
	index := start.
	[ (element := array at: (index := index \\ array size + 1)) == nil ] whileFalse: [
		| newIndex |
		(newIndex := self findElementOrNil: element key) = index ifFalse: [
			array swap: index with: newIndex ] ]
]

{ #category : #'flat collect' }
Dictionary >> flatCollect: aBlock [
	"Evaluate aBlock for each of the receiver's values (by opposition to keys) and answer the
	list of all resulting values flatten one level. Assumes that aBlock returns some kind
	of collection for each element. Equivalent to the lisp's mapcan"
	"If you want to have keys use associations collect: or associations flatCollect: "

	^ self flatCollect: aBlock as: OrderedCollection
]

{ #category : #testing }
Dictionary >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a key that begins with aString, false otherwise"

	self keysDo:[:each |
		(each beginsWith: aString)
			ifTrue:[^true]].
	^false
]

{ #category : #testing }
Dictionary >> includes: anObject [

	"Check if anObject is one of the values in the dictionary"

	self do: [ :each | anObject = each ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #testing }
Dictionary >> includesAssociation: anAssociation [
  ^ (self
      associationAt: anAssociation key
      ifAbsent: [ ^ false ]) value = anAssociation value
]

{ #category : #testing }
Dictionary >> includesIdentity: anObject [
	"Answer whether anObject is one of the values of the receiver.  Contrast #includes: in which there is only an equality check, here there is an identity check"

	self do: [:each | anObject == each ifTrue: [^ true]].
	^ false
]

{ #category : #testing }
Dictionary >> includesKey: key [
	"Answer whether the receiver has a key equal to the argument, key."

	^ (array at: (self scanFor: key)) ~~ nil
	"We could use #notNil here, but ProtoObject doesn't understand it."
]

{ #category : #testing }
Dictionary >> isDictionary [
	^true
]

{ #category : #testing }
Dictionary >> isHealthy [
	"Test that object hashes match their positions stored in set's array,
	answer true if everything ok, false otherwise

	Dictionary allInstances select: [:dict |
		dict isHealthy not ]
	Dictionary allSubInstances select: [:dict |
		dict isHealthy not ]
	"
	array withIndexDo: [:elem :i |
		elem ifNotNil: [
			(self scanFor: elem key) == i ifFalse: [ ^ false ]
			]
	].
	^ true
]

{ #category : #accessing }
Dictionary >> keyAtIdentityValue: value [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer nil.
	Note: There can be multiple keys with the same value. Only one is returned."

	^self keyAtIdentityValue: value ifAbsent: [self errorValueNotFound: value]
]

{ #category : #accessing }
Dictionary >> keyAtIdentityValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer the result of evaluating exceptionBlock.
	Note: There can be multiple keys with the same value. Only one is returned."

	self associationsDo:
		[:association | value == association value ifTrue: [^association key]].
	^exceptionBlock value
]

{ #category : #accessing }
Dictionary >> keyAtValue: value [
	"Answer the key that is the external name for the argument, value. If
	there is none, signal an error."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound: value]
]

{ #category : #accessing }
Dictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer the result of evaluating exceptionBlock.
	: Use =, not ==, so stings like 'this' can be found.  Note that MethodDictionary continues to use == so it will be fast."

	self associationsDo:
		[:association | value = association value ifTrue: [^association key]].
	^exceptionBlock value
]

{ #category : #accessing }
Dictionary >> keyForIdentity: anObject [
	"If anObject is one of the values of the receive, return its key, else return nil.  Contrast #keyAtValue: in which there is only an equality check, here there is an identity check"

	self associationsDo: [:assoc | assoc value == anObject ifTrue: [^ assoc key]].
	^ nil
]

{ #category : #accessing }
Dictionary >> keys [
	"Answer an Array containing the receiver's keys."

	^Array new: self size streamContents: [:s| self keysDo: [:key| s nextPut: key]]
]

{ #category : #enumerating }
Dictionary >> keysAndValuesDo: aBlock [

	"
	Evaluate aBlock for each of the receiver's keys and values. The block must accept two
	inputs, the first being the key and the second being the value. See keysDo: or valuesDo:
	if you only need to evaluate one or the other. Also see associationsDo: for a similar
	selector where the block accepts a single agrument.

	```
	d := Dictionary withAll: #(4 5 9 6 76).
	a := OrderedCollection new.
	d keysAndValuesDo: [ :k :v | a add: v. a add: k ]. a ""(76 5 9 3 4 1 6 4 5 2)""
	```
	"

	^ self associationsDo: [ :assoc |
		  aBlock value: assoc key value: assoc value ]
]

{ #category : #removing }
Dictionary >> keysAndValuesRemove: keyValueBlock [
	"Removes all entries for which keyValueBlock returns true."
	"When removing many items, you must not do it while iterating over the dictionary, since it may be changing.  This method takes care of tallying the removals in a first pass, and then performing all the deletions afterward.  Many places in the sytem could be simplified by using this method."

	| removals |
	removals := OrderedCollection new.
	self associationsDo:
		[:assoc | (keyValueBlock value: assoc key value: assoc value)
			ifTrue: [removals add: assoc key]].
 	removals do:
		[:aKey | self removeKey: aKey]
]

{ #category : #enumerating }
Dictionary >> keysDo: aBlock [

	"Evaluate aBlock for each of the receiver's keys.


	Iterating through the keys does not create a new dictionary but you can save the results
	to another collection

	```
	d := Dictionary withAll: #(4 5 9 6 76).
	a := OrderedCollection new.
	d keysDo: [ :each |  a add: each ]. ""an OrderedCollection(5 3 1 4 2)""
	```

	It's also possible to modify the dictionary while iterating through the keys

	```
	d := Dictionary withAll: #(4 5 9 6 76).
	d keysDo: [ :each | d at: each put: (d at: each) - 1 ]  ""a Dictionary(1->3 2->4 3->8 4->5 5->75 )""
	```
	"

	self associationsDo: [ :association | aBlock value: association key ]
]

{ #category : #accessing }
Dictionary >> keysSortedSafely [
	"Answer an Array containing the receiver's keys."
 	"Suggested by l. Uzonyi"

 	| sortedKeys |
 	sortedKeys := Array
						new: self size
						streamContents: [ :stream |
 								self keysDo: [ :each | stream nextPut: each ] ].
 	sortedKeys sort: [ :x :y |
 		"Should really be use <obj, string, num> compareSafely..."
 		((x isString and: [ y isString ])
 			or: [ x isNumber and: [ y isNumber ] ])
 			ifTrue: [ x < y ]
 			ifFalse: [ x class == y class
 				ifTrue: [ x printString < y printString ]
 				ifFalse: [ x class name < y class name ] ] ].
 	^sortedKeys
]

{ #category : #private }
Dictionary >> noCheckAdd: anObject [
	"Must be defined separately for Dictionary because (self findElementOrNil:) expects a key, not an association."

	array at: (self findElementOrNil: anObject key) put: anObject.
	tally := tally + 1
]

{ #category : #private }
Dictionary >> noCheckNoGrowFillFrom: anArray [
	"Add the elements of anArray except nils to me assuming that I don't contain any of them, they are unique and I have more free space than they require."

	1 to: anArray size do: [ :index |
		(anArray at: index) ifNotNil: [ :association |
			array
				at: (self scanForEmptySlotFor: association key)
				put: association ] ]
]

{ #category : #copying }
Dictionary >> postCopy [
	"Must copy the associations, or later store will affect both the
original and the copy"

	array := array collect: [ :association |
		association ifNotNil: [ association copy ] ]
]

{ #category : #printing }
Dictionary >> printElementsOn: aStream [
	aStream nextPut: $(.
	self size > 100
		ifTrue: [aStream nextPutAll: 'size '.
			self size printOn: aStream]
		ifFalse: [self keysSortedSafely
				do: [:key | aStream print: key;
						 nextPutAll: '->';
						 print: (self at: key);
						 space]].
	aStream nextPut: $)
]

{ #category : #private }
Dictionary >> rehash [
	"Smalltalk rehash."
	| newSelf |
	newSelf := self species new: self size.
	self associationsDo: [:each | newSelf noCheckAdd: each].
	array := newSelf array
]

{ #category : #removing }
Dictionary >> remove: anObject [

	self shouldNotImplement
]

{ #category : #removing }
Dictionary >> remove: anObject ifAbsent: exceptionBlock [

	self shouldNotImplement
]

{ #category : #removing }
Dictionary >> removeKey: key [
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #removing }
Dictionary >> removeKey: key ifAbsent: aBlock [
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."

	| index assoc |
	index := self findElementOrNil: key.
	assoc := array at: index.
	assoc == nil ifTrue: [ ^ aBlock value ].
	array at: index put: nil.
	tally := tally - 1.
	self fixCollisionsFrom: index.
	^ assoc value
]

{ #category : #undeclared }
Dictionary >> removeUnreferencedKeys [
	"This is private code for the Undeclared handling, not a general API."
	"Undeclared removeUnreferencedKeys"

	^ self unreferencedKeys do: [ :key | self removeKey: key ]
]

{ #category : #private }
Dictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key = anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]

{ #category : #enumerating }
Dictionary >> select: aBlock [

	"Evaluate aBlock with each of my values as the argument. Collect into a new dictionary, only those associations for which aBlock evaluates to true.

	```
	d := Dictionary withAll: #(1 2 3 4 5). ""a Dictionary(1->1 2->2 3->3 4->4 5->5 )""
	d select: [ :each | each even ]. ""a Dictionary(2->2 4->4 )""
	```
	"

	| newCollection |
	newCollection := self copyEmpty.
	self associationsDo: [ :each |
		(aBlock value: each value) ifTrue: [ newCollection add: each copy ] ].
	^ newCollection
]

{ #category : #printing }
Dictionary >> storeOn: aStream [
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self associationsDo:
			[:each |
			noneYet
				ifTrue: [noneYet := false]
				ifFalse: [aStream nextPut: $;].
			aStream nextPutAll: ' add: '.
			aStream store: each].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : #undeclared }
Dictionary >> unreferencedKeys [
	"This is private code for the Undeclared handling.
 	Optimize for performance--examine each literal only once.
 	Since we know these are variable bindings, they cannot be
 	optimized selectors nor be elements of Arrays, but we do need
 	to traverse nested blocks."

 	| unreferenced |
 	unreferenced := self keys asSet.
 	self systemNavigation allBehaviorsDo: [ :class |
 		class methodsDo: [ :method |
 			method withAllNestedLiteralsDo: [ :lit |
 				(lit isKindOf: UndeclaredVariable) ifTrue: [
 					unreferenced
 						remove: lit key
 						ifAbsent: [ "Sanity check--a binding with more than one reference may already be gone from the unreferenced set, but should be present in Undeclared itself"
 							self at: lit key ] ] ] ] ].
 	^ unreferenced
]

{ #category : #private }
Dictionary >> valueAtNewKey: aKey put: anObject atIndex: index declareFrom: aDictionary [
	"Support for coordinating class variable and global declarations
	with variables that have been put in Undeclared so as to
	redirect all references to the undeclared variable."

	(aDictionary includesKey: aKey)
		ifTrue:
			[self atNewIndex: index
				put: ((aDictionary associationAt: aKey) value: anObject).
			aDictionary removeKey: aKey]
		ifFalse:
			[self atNewIndex: index put: (Association key: aKey value: anObject)]
]

{ #category : #accessing }
Dictionary >> values [
	"Answer a Collection containing the receiver's values."
	^Array
		new: self size
		streamContents: [ :out | self valuesDo: [:value | out nextPut: value]]
]

{ #category : #enumerating }
Dictionary >> valuesDo: aBlock [

	"Evaluate aBlock for each of the receiver's values.  Implemented with == checks
	merely for the sake of maximum efficiency


	The result of any transformation of each value is not reflected in the dictionary.
	If you wish to do this use the select: message.

	```
	d := Dictionary withAll: #(1 2 3 4 5).
	d valuesDo: [ :each | each * 7 ].  ""a Dictionary(1->1 2->2 3->3 4->4 5->5 )""
	```

	However you can assign results to another collection in the block if you want.
	(Note that the ordering isn't guarenteed)

	```
	a := OrderedCollection new.
	d valuesDo: [ :each | a add: each * 7 ].  ""an OrderedCollection(35 21 7 28 14)""
	```
	"

	tally = 0 ifTrue: [ ^ self ].
	1 to: array size do: [ :eachIndex |
		| eachAssociation |
		eachAssociation := array at: eachIndex.
		nil == eachAssociation ifFalse: [
			aBlock value: eachAssociation value ] ]
]
