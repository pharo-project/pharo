"
`SmallDictionary` is a special dictionary optimized for small collections. In addition to the normal dictionary protocol, it also supports an `#empty` message which ""empties"" the collection but may hang on to the original elements (so it could collect garbage). Without `#empty` we would either need to create a new dictionary or explicitly remove everything from the dictionary. Both of these take more time and `#empty`. Be careful, I cannot have *nil* as key. 

### Public API and Key Messages

- #at: aKey put: aValue / #at: aKey ifAbsentPut: aValue 		allow to add an element.  
- #at: aKey / #at: aKey ifAbsent: aBlock / #at: aKey ifPresent: aBlock ifAbsent: aBlock 		allow to access my values.
- #keysDo: aBlock / #valuesDo: aBlock / #associationsDo: 		allow to iterate on me effectively

### Examples 


To create a dictiony with indexes as key: 

```
SmallDictionary withAll: #(7 3 1 3)   		
"">>>  a SmallDictionaryDictionary(1->7 2->3 3->1 4->3 ""
```
To use Objects as key (here symbols): 

```
	colors := SmallDictionary new 
				at: #yellow put: Color yellow; 
				at: #blue put: Color blue;
				at: #red put: Color red;
				yourself.
				
	colors at: #yellow. 	""returns:  Color yellow""
	colors keys          		""returns: a Set(#blue #yellow #red)""
	colors values     		""returns:  {Color blue. Color yellow. Color red}""

	colors empty 	""a SmallDictionary()""
```

### Internal Representation and Key Implementation Points.
Instance Variables
- keys:		<Array>		Array of keys (we don't use Associations for our key value pairs)
- size:			<Integer>	Size of the dictionary
- values:		<Array>		Array of our values


    Implementation Points
"
Class {
	#name : #SmallDictionary,
	#superclass : #Collection,
	#instVars : [
		'keys',
		'values',
		'size'
	],
	#category : #'Collections-Unordered-Dictionaries'
}

{ #category : #'instance creation' }
SmallDictionary class >> new: aSize [
	"Ignore the size"

	^self basicNew initialize
]

{ #category : #'instance creation' }
SmallDictionary class >> newFrom: aDict [
	"Answer an instance of me containing the same associations as aDict.
	 Error if any key appears twice."
	| newDictionary |
	newDictionary := self new: aDict size.
	aDict associationsDo:
		[:x |
		(newDictionary includesKey: x key)
			ifTrue: [self error: 'Duplicate key: ', x key printString]
			ifFalse: [newDictionary add: x]].
	^ newDictionary
]

{ #category : #'instance creation' }
SmallDictionary class >> newFromKeys: keys andValues: values [
	"Create a dictionary from the keys and values arguments which should have the same length."

	"(SmallDictionary newFromKeys: #(#x #y) andValues: #(3 6)) >>> (SmallDictionary new at: #x put: 3; at: #y put: 6 ;yourself)"

	| dict |
	dict := self new.
	keys with: values do: [ :k :v | dict at: k put: v ].
	^ dict
]

{ #category : #'instance creation' }
SmallDictionary class >> newFromPairs: anArray [
	"Answer an instance of me associating (anArray at:i) to (anArray at: i+i) for each odd i.  anArray must have an even number of entries."

	| newDictionary |

	newDictionary := self new: (anArray size/2).
	1 to: (anArray size-1) by: 2 do: [ :i|
		newDictionary at: (anArray at: i) put: (anArray at: i+1).
	].
	^ newDictionary
]

{ #category : #comparing }
SmallDictionary >> = aDictionary [
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
SmallDictionary >> add: anAssociation [
	self at: anAssociation key put: anAssociation value.
	^anAssociation
]

{ #category : #adding }
SmallDictionary >> addAll: aKeyedCollection [
	aKeyedCollection == self
		ifFalse: [
			aKeyedCollection keysAndValuesDo: [:key :value | self at: key put: value]].
	^aKeyedCollection
]

{ #category : #accessing }
SmallDictionary >> associationAt: key [
	"Returns an association for the given key.

	Modifying the association won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	^ self associationAt: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #accessing }
SmallDictionary >> associationAt: key ifAbsent: aBlock [
	"Answer an association for the given key.
	If the key is not found, return the result of evaluating aBlock.

	Modifying the association won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	| index value |
	index := keys indexOf: key.
	index = 0 ifTrue: [ ^ aBlock value].

	value := values at: index.
	^ key->value
]

{ #category : #accessing }
SmallDictionary >> associationAt: key ifPresent: aBlock [
	"Answer the value of evaluating aBlock optionally with an association
	for the given key.
	If the key is not found, return nil.

	Modifying the association won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	| index |
	(index := keys indexOf: key) = 0
		ifTrue: [ ^ nil ].
	^ aBlock cull: key -> (values at: index)
]

{ #category : #accessing }
SmallDictionary >> associationAt: key ifPresent: aPresentBlock ifAbsent: anAbsentBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the association for the key.
	Otherwise answer the value of the second block."

	self associationAt: key ifPresent: [:assoc | ^ aPresentBlock cull: assoc].
	^ anAbsentBlock value
]

{ #category : #accessing }
SmallDictionary >> associations [
	"Answer a collection containing associations for the receiver.
	Suggested by l. Uzonyi

	Modifying the associations won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	^Array new: self size streamContents: [ :stream |
 		self associationsDo: [ :each | stream nextPut: each ] ]
]

{ #category : #enumerating }
SmallDictionary >> associationsDo: aBlock [
	"Evaluate aBlock for each association for the receiver.

	Modifying the associations won't affect the receiver because it
	isn't implemented with associations like Dictionary."

	self keysAndValuesDo: [:key :value | aBlock value: key -> value]
]

{ #category : #enumerating }
SmallDictionary >> associationsSelect: aBlock [
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
SmallDictionary >> at: key [
	"Answer the value associated with the key."

	^ self at: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #'nested dictionaries' }
SmallDictionary >> at: firstKey at: secondKey [
	"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey."

	"
	(Dictionary new
		at: #top at: #below1 put: 1;
		at: #top at: #below1 put: 2;
		at: #top at: #below1)
	>>>
	2"

	^ self at: firstKey at: secondKey ifAbsent: [ self errorKeyNotFound: secondKey ]
]

{ #category : #'nested dictionaries' }
SmallDictionary >> at: firstKey at: secondKey ifAbsent: aZeroArgBlock [
		"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey. Execute aZeroArgBlock in case one of the key is wrong."

	| subDictionary |
	subDictionary := self at: firstKey ifAbsent: [ ^ aZeroArgBlock value ].
	^ subDictionary at: secondKey ifAbsent: aZeroArgBlock
]

{ #category : #'nested dictionaries' }
SmallDictionary >> at: firstKey at: secondKey ifAbsentPut: aZeroArgBlock [
	"Return the object stored in the second dictionary at secondKey. The second dictionary is accessed via the key firstKey. If firstKey is not defined, set a new dictionary for the second key and set the value of aZeroArgBlock execution. If firstKey is defined and not second key set the value of aZeroArgBlock execution. See NestedDictionaryTest for examples."

	| subDictionary |
	subDictionary := self at: firstKey ifAbsentPut: [ self species new ].
	^ subDictionary at: secondKey ifAbsentPut: aZeroArgBlock
]

{ #category : #'nested dictionaries' }
SmallDictionary >> at: firstKey at: secondKey put: aValue [
	"Set a value at secondKey in the dictionary returned by firstKey."

	| subDictionary |
	subDictionary := self at: firstKey ifAbsentPut: [ self species new ].
	^ subDictionary at: secondKey put: aValue
]

{ #category : #accessing }
SmallDictionary >> at: key ifAbsent: aBlock [
	"Answer the value associated with the key or, if key isn't found,
	answer the result of evaluating aBlock."

	| index |
	index := self findIndexForKey:  key.
	index = 0 ifTrue: [^ aBlock value].

	^ values at: index.

	"| assoc |
	assoc := array at: (self findElementOrNil: key).
	assoc ifNil: [^ aBlock value].
	^ assoc value"
]

{ #category : #accessing }
SmallDictionary >> at: key ifAbsentPut: aBlock [
	"Return the value at the given key.
	If the key is not included in the receiver store and return the result
	of evaluating aBlock as the new value."

	| index |
	index := self findIndexForKey:  key.
	^ index = 0
		ifFalse: [values at: index]
		ifTrue: [self privateAt: key put: aBlock value]
]

{ #category : #accessing }
SmallDictionary >> at: key ifPresent: aBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the given block optionally with the value associated
	with the key.
	Otherwise, answer nil."

	| v |
	v := self at: key ifAbsent: [^ nil].
	^ aBlock cull: v
]

{ #category : #accessing }
SmallDictionary >> at: key ifPresent: aPresentBlock ifAbsent: anAbsentBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the value associated
	with the key.
	Otherwise answer the value of the second block."

	self at: key ifPresent: [ :v | ^ aPresentBlock cull: v ].
	^ anAbsentBlock value
]

{ #category : #accessing }
SmallDictionary >> at: key ifPresent: aPresentBlock ifAbsentPut: anAbsentBlock [
	"Lookup the given key in the receiver. If it is present, answer the
	value of evaluating the first block optionally with the value associated
	with the key.
	Otherwise store and return the result of evaluating the second block as the
	new value of the key."

	^ self
		at: key
		ifPresent: aPresentBlock
		ifAbsent: [ self at: key put: anAbsentBlock value ]
]

{ #category : #putting }
SmallDictionary >> at: key put: value [
	"Set the value at key to be anObject.  If key is not found, create a
	new entry for key and set is value to anObject. Answer anObject."

	| index |
	index := self findIndexForKey:  key.
	^ index = 0
		ifFalse: [values at: index put: value]
		ifTrue: [self privateAt: key put: value]
]

{ #category : #accessing }
SmallDictionary >> at: key update: updateBlock [
	"I am used to update the value at a given key, or if the key does not exist, to throw an error"
	self at: key update: updateBlock initial: [ self errorKeyNotFound: key ]
]

{ #category : #accessing }
SmallDictionary >> at: key update: updateBlock initial: initBlocktOrValue [
	"I am used to update the value at a given key. The updateBlock is passed
	the existing value, and the result of the block is stored back.
	If the key does not exist, store the value of the initBlocktOrValue.
	initBlocktOrValue can be a block in case the initial value is expencive to compute.
	I use findElementOrNil: to avoid looking up the key twice."
	| index |
	index := self findIndexForKey: key.
	(index = 0)
		ifTrue:  [ self at: key put: initBlocktOrValue value]
		ifFalse: [ (values at: index) in: [ :val | values at: index put: (updateBlock value: val) ] ]
]

{ #category : #accessing }
SmallDictionary >> bindingOf: varName [
	^self associationAt: varName ifAbsent:[nil]
]

{ #category : #enumerating }
SmallDictionary >> bindingsDo: aBlock [
	self associationsDo: aBlock
]

{ #category : #accessing }
SmallDictionary >> capacity [
	^keys size
]

{ #category : #enumerating }
SmallDictionary >> collect: aBlock [
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
SmallDictionary >> declare: key from: aDictionary [
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
SmallDictionary >> difference: aCollection [
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
SmallDictionary >> do: aBlock [

	^ self valuesDo: aBlock
]

{ #category : #accessing }
SmallDictionary >> empty [
	1 to: size do: [ :index |
		keys at: index put: nil.
		values at: index put: nil ].
	size := 0
]

{ #category : #private }
SmallDictionary >> errorKeyNotFound: aKey [

	KeyNotFound signalFor: aKey
]

{ #category : #private }
SmallDictionary >> errorValueNotFound: value [

	ValueNotFound signalFor: value
]

{ #category : #private }
SmallDictionary >> findIndexForKey: aKey [
	^ keys indexOf: aKey
]

{ #category : #private }
SmallDictionary >> growKeysAndValues [
	self growTo: size * 2
]

{ #category : #private }
SmallDictionary >> growTo: aSize [
	| newKeys newValues |
	newKeys := Array new: aSize.
	newValues := Array new: aSize.
	1 to: size
		do:
			[:i |
			newKeys at: i put: (keys at: i).
			newValues at: i put: (values at: i)].
	keys := newKeys.
	values := newValues
]

{ #category : #testing }
SmallDictionary >> hasBindingThatBeginsWith: aString [
	"Answer true if the receiver has a key that begins with aString, false otherwise"

	self keysDo:[:each |
		(each beginsWith: aString)
			ifTrue:[^true]].
	^false
]

{ #category : #testing }
SmallDictionary >> includes: aValue [
	self do: [:each | aValue = each ifTrue: [^true]].
	^false
]

{ #category : #testing }
SmallDictionary >> includesAssociation: anAssociation [
  ^ (self
      associationAt: anAssociation key
      ifAbsent: [ ^ false ]) value = anAssociation value
]

{ #category : #testing }
SmallDictionary >> includesIdentity: aValue [
	"Answer whether aValue is one of the values of the receiver.  Contrast #includes: in which there is only an equality check, here there is an identity check"

	self do: [:each | aValue == each ifTrue: [^ true]].
	^ false
]

{ #category : #testing }
SmallDictionary >> includesKey: key [
	"Answer whether the receiver has a key equal to the argument, key."

	^ (self findIndexForKey: key) ~= 0
]

{ #category : #initialization }
SmallDictionary >> initialize [
	super initialize.
	keys := Array new: 2.
	values := Array new: 2.
	size := 0
]

{ #category : #testing }
SmallDictionary >> isDictionary [
	^true
]

{ #category : #testing }
SmallDictionary >> isHealthy [
	"Since this dictionary does no hashing, we consider it healthy
	if it contains no duplicate keys."

	| uniqueKeys |
	uniqueKeys := self setClass new: self size.
	keys
		do: [ :each |
			each
				ifNotNil: [
					(uniqueKeys includes: each)
						ifTrue: [ ^ false ].
					uniqueKeys add: each ] ].
	^ true
]

{ #category : #accessing }
SmallDictionary >> keyAtIdentityValue: value [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer nil.
	Note: There can be multiple keys with the same value. Only one is returned."

	^self keyAtIdentityValue: value ifAbsent: [self errorValueNotFound: value]
]

{ #category : #accessing }
SmallDictionary >> keyAtIdentityValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer the result of evaluating exceptionBlock.
	Note: There can be multiple keys with the same value. Only one is returned."

	| index |
	index := (values identityIndexOf: value).
	index = 0
		ifTrue: [ ^ exceptionBlock value].
	^ keys at: index
]

{ #category : #accessing }
SmallDictionary >> keyAtValue: value [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer nil."

	^self keyAtValue: value ifAbsent: [self errorValueNotFound: value]
]

{ #category : #accessing }
SmallDictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer the result of evaluating exceptionBlock.
	: Use =, not ==, so stings like 'this' can be found.  Note that MethodDictionary continues to use == so it will be fast."

	| index |
	index := (values indexOf: value).
	index = 0
		ifTrue: [ ^ exceptionBlock value].

	^ keys at: index
]

{ #category : #accessing }
SmallDictionary >> keyForIdentity: aValue [
	"If aValue is one of the values of the receive, return its key, else return nil.  Contrast #keyAtValue: in which there is only an equality check, here there is an identity check"

	self keysAndValuesDo: [:key :value |  value == aValue ifTrue: [^  key]].
	^ nil
]

{ #category : #accessing }
SmallDictionary >> keys [
	"Answer an Array containing the receiver's keys."

	^ keys copyFrom: 1 to: size
]

{ #category : #enumerating }
SmallDictionary >> keysAndValuesDo: aBlock [
	1 to: size do: [:i | aBlock value: (keys at: i) value: (values at: i)]
]

{ #category : #removing }
SmallDictionary >> keysAndValuesRemove: keyValueBlock [
	"Removes all entries for which keyValueBlock returns true."
	"When removing many items, you must not do it while iterating over the dictionary, since it may be changing.  This method takes care of tallying the removals in a first pass, and then performing all the deletions afterward.  Many places in the sytem could be simplified by using this method."

	| removals |
	removals := OrderedCollection new.
	self keysAndValuesDo:
		[:key :value | (keyValueBlock value:  key value:  value)
			ifTrue: [removals add:  key]].
 	removals do:
		[:aKey | self removeKey: aKey]
]

{ #category : #enumerating }
SmallDictionary >> keysDo: aBlock [
	1 to: size do: [:i | aBlock value: (keys at: i)]
]

{ #category : #accessing }
SmallDictionary >> keysSortedSafely [
	"Answer a SortedCollection containing the receiver's keys."
	| sortedKeys |
	sortedKeys := SortedCollection new: self size.
	sortedKeys sortBlock:
		[:x :y |  "Should really be use <obj, string, num> compareSafely..."
		((x isString and: [y isString])
			or: [x isNumber and: [y isNumber]])
			ifTrue: [x < y]
			ifFalse: [x class == y class
				ifTrue: [x printString < y printString]
				ifFalse: [x class name < y class name]]].
	self keysDo: [:each | sortedKeys addLast: each].
	^ sortedKeys reSort
]

{ #category : #copying }
SmallDictionary >> postCopy [
	keys := keys copy.
	values := values copy
]

{ #category : #printing }
SmallDictionary >> printElementsOn: aStream [
	| noneYet |
	aStream nextPut: $(.
	noneYet := true.
	self associationsDo: [ :each |
			noneYet
				ifTrue: [ noneYet := false ]
				ifFalse: [ aStream space ].
			aStream print: each].
	aStream nextPut: $)
]

{ #category : #private }
SmallDictionary >> privateAt: key put: value [
	size == keys size ifTrue: [self growKeysAndValues].
	size := size + 1.
	keys at: size put: key.
	^values at: size put: value
]

{ #category : #removing }
SmallDictionary >> remove:anAssociation [

	self removeKey:anAssociation key
]

{ #category : #removing }
SmallDictionary >> remove: oldObject ifAbsent: anExceptionBlock [
	self removeKey: oldObject key ifAbsent: anExceptionBlock.
	^oldObject
]

{ #category : #removing }
SmallDictionary >> removeAll [
	self initialize
]

{ #category : #removing }
SmallDictionary >> removeKey: key [
	"Remove key from the receiver.
	If key is not in the receiver, notify an error."

	^ self removeKey: key ifAbsent: [self errorKeyNotFound: key]
]

{ #category : #removing }
SmallDictionary >> removeKey: key ifAbsent: aBlock [
	"Remove key (and its associated value) from the receiver. If key is not in
	the receiver, answer the result of evaluating aBlock. Otherwise, answer
	the value externally named by key."

	| index value |
	index := self findIndexForKey:  key.
	index = 0 ifTrue: [^aBlock value].

	value := values at: index.
	index to: size - 1
		do:
			[:i |
			keys at: i put: (keys at: i + 1).
			values at: i put: (values at: i + 1)].
	keys at: size put: nil.
	values at: size put: nil.
	size := size - 1.
	^value
]

{ #category : #enumerating }
SmallDictionary >> select: aBlock [
	"Evaluate aBlock with each of my values as the argument. Collect into a
	new dictionary, only those associations for which aBlock evaluates to
	true."

	| newCollection |
	newCollection := self copyEmpty.
	self associationsDo:
		[:each |
		(aBlock value: each value) ifTrue: [newCollection add: each]].
	^newCollection
]

{ #category : #private }
SmallDictionary >> setClass [
	^ Set
]

{ #category : #accessing }
SmallDictionary >> size [
	^size
]

{ #category : #printing }
SmallDictionary >> storeOn: aStream [
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' new)'.
	noneYet := true.
	self associationsDo: [ :each |
			noneYet
				ifTrue: [ noneYet := false ]
				ifFalse: [ aStream nextPut: $; ].
			aStream nextPutAll: ' add: '.
			aStream store: each].
	noneYet ifFalse: [ aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : #accessing }
SmallDictionary >> values [
	"Answer a Collection containing the receiver's values."
	^Array
		new: self size
		streamContents: [ :out | self valuesDo: [:value | out nextPut: value]]
]

{ #category : #enumerating }
SmallDictionary >> valuesDo: aBlock [
	"Evaluate aBlock for each of the receiver's values."

1 to: size do: [:i | aBlock value: (values at: i)]
]
