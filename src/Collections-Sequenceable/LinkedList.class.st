"
I am a sequential collection where adjecent objects are linked.
I can store any kind of objects that I will wrap into a `Link` (See implementation points).
I act pretty much as an OrderedCollection, but my access times differs: the method `#atIndex:` is depending of the number of elements so is not constant. In the worse case, the whole list must be crossed.
Using the message `#addFirst:` / `#removeLast` causes the receiver to behave as a stack; using `#addLast:` / `#removeFirst` causes the receiver to behave as a queue.

I use the `Link` class (or his subclasses) in my implementation. 

## Public API and Key Messages

- `#new` / `#withAll: aCollection` / `#with: anObject` 	are common constructors
- `#add: anObject` / `#at: anIndex put: anObject` 	allow to add new elements to myself.
- `#remove: anObject` 	allow to remove an element.
- `#do: aBlock` / `#collect: aBlock` / `#select: aBlock` / `#reject: aBlock` 	are common iterators.

### Adding 

There is many ways to create a LinkedList, they can be created from scratch by initializing with `#new` and then adding elements progressively:
```
	linkedList := LinkedList new.
	linkedList
		add: 'one';
		add: 'two';
		addFirst: 'zero';
		addLast: 'three'.
	linkedList.		""returns: a LinkedList('zero' 'one' 'two' 'three')""
```
Or, by adding individual objects using `#with:`
```
	
	linkedList := LinkedList with: 'one' with: 'two' with: 'three'.
	linkedList.		""returns: a LinkedList('one' 'two' 'three')""
```
Or, by adding another collection using `#withAll:`
```
	""add all objects from Array""
	linkedList := LinkedList withAll: #('one' 'two' 'three').
	linkedList.		""returns: a LinkedList('one' 'two' 'three')""
```
### Removing
```
	linkedList := LinkedList new.
	linkedList
		add: 1;
		add: 2.
	linkedList.		""returns: a LinkedList(1 2)""
	linkedList remove: 1.
	linkedList.		""returns: a LinkedList(2)""
```
### Enumerating
```
	linkedList := LinkedList with: $b with: $c with: $a.
	linkedList sort: [ :first :second | first < second ].			""returns: a LinkedList($a $b $c)""
	linkedList collect: [ :element | element asUppercase ].	""returns:  a LinkedList($A $B $C)""
	linkedList select: [ :element | element >= $b ].			""returns:  a LinkedList($b $c)""
	linkedList do: [ :element | element inspect ].
	linkedList asArray									""returns: #($a $b $c)""
```
			
### Internal Representation and Key Implementation Points.

Instance Variables
- **firstLink <Link>**: 	A link that contains the first value of the LinkedList.
- **lastLink <Link>**: 	A link that contains the last value of the LinkedList.

If you attempt to add any object into a `LinkedList` that is not a `Link`, it will automatically be wrapped by a `ValueLink`.
"
Class {
	#name : #LinkedList,
	#superclass : #SequenceableCollection,
	#instVars : [
		'firstLink',
		'lastLink'
	],
	#category : #'Collections-Sequenceable-Base'
}

{ #category : #'instance creation' }
LinkedList class >> new: anInt [
	"LinkedList don't need capacity"
	^self new
]

{ #category : #'stream creation' }
LinkedList class >> new: size streamContents: aBlock [
	^ self withAll: (super new: size streamContents: aBlock)
]

{ #category : #'instance creation' }
LinkedList class >> newFrom: aCollection [
	"Answer an instance with same elements as aCollection."
	^self new
		addAll: aCollection;
		yourself
]

{ #category : #accessing }
LinkedList class >> streamSpecies [
	^ Array
]

{ #category : #adding }
LinkedList >> add: aLinkOrObject [
	"Add aLink to the end of the receiver's list. Answer aLink."

	^self addLast: aLinkOrObject
]

{ #category : #adding }
LinkedList >> add: link after: otherLinkOrObject [
	"Add otherLink  after link in the list. Answer aLink."

	| otherLink |
	otherLink := self linkAt: (self indexOf: otherLinkOrObject).
	^ self add: link afterLink: otherLink
]

{ #category : #adding }
LinkedList >> add: aLinkOrObject afterLink: otherLink [

	"Add otherLink  after link in the list. Answer aLink."

	| savedLink aLink |
	lastLink == otherLink ifTrue: [^ self addLast: aLinkOrObject].
	savedLink := otherLink nextLink.
	aLink := aLinkOrObject asLink.
	otherLink nextLink: aLink.
	aLink nextLink:  savedLink.
	^aLink
]

{ #category : #adding }
LinkedList >> add: link before: otherLinkOrObject [
	"Add otherLink  after link in the list. Answer aLink."

	| otherLink |
	otherLink := self linkAt: (self indexOf: otherLinkOrObject).
	^ self add: link beforeLink: otherLink
]

{ #category : #adding }
LinkedList >> add: aLinkOrObject beforeLink: otherLink [

	| currentLink|

	firstLink == otherLink ifTrue: [^ self addFirst: aLinkOrObject].

	currentLink := firstLink.
	[currentLink == nil] whileFalse: [
		currentLink nextLink == otherLink ifTrue: [
			| aLink |
			aLink := aLinkOrObject asLink.
			aLink nextLink: currentLink nextLink.
			currentLink nextLink: aLink.
			^ aLink
		].
		 currentLink := currentLink nextLink.
	].
	^ self errorNotFound: otherLink
]

{ #category : #adding }
LinkedList >> addFirst: aLinkOrObject [
	"Add aLink to the beginning of the receiver's list. Answer aLink."
	|aLink|
	aLink := aLinkOrObject asLink.
	self isEmpty ifTrue: [lastLink := aLink].
	aLink nextLink: firstLink.
	firstLink := aLink.
	^aLink
]

{ #category : #adding }
LinkedList >> addLast: aLinkOrObject [
	"Add aLink to the end of the receiver's list. Answer aLink."
	|aLink|
	aLink := aLinkOrObject asLink.
	self isEmpty
		ifTrue: [firstLink := aLink]
		ifFalse: [lastLink nextLink: aLink].
	lastLink := aLink.
	^aLink
]

{ #category : #accessing }
LinkedList >> at: index [

	^(self linkAt: index) value
]

{ #category : #accessing }
LinkedList >> at: index put: anObject [

	^self at: index putLink: (self linkOf: anObject ifAbsent: [anObject asLink])
]

{ #category : #accessing }
LinkedList >> at: index putLink: aLink [
	| previousLink nextLink |
	"Please don't put a link which is already in the list, or you will create an infinite loop"
	(self validIndex: index)
		ifFalse: [^ self errorOutOfBounds].
	index = 1
		ifTrue: [aLink nextLink: self firstLink nextLink.
			firstLink := aLink.
			aLink nextLink ifNil: [lastLink := aLink].
			^ aLink].
	previousLink := self linkAt: index - 1.
	nextLink := previousLink nextLink nextLink.

	nextLink ifNil: [
		aLink nextLink: self lastLink
	] ifNotNil: [
		aLink nextLink: nextLink.
	].

	previousLink nextLink: aLink.

	nextLink ifNil: [
		lastLink := aLink.
		aLink nextLink: nil.
	].

	^ aLink
]

{ #category : #enumerating }
LinkedList >> collect: aBlock [
	"Evaluate aBlock with each of the receiver's elements as the argument.
	Collect the resulting values into a collection like the receiver. Answer
	the new collection."

	| aLink newCollection |
	newCollection := self class new.
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[newCollection add: (aBlock value: aLink value).
		 aLink := aLink nextLink].
	^ newCollection
]

{ #category : #enumerating }
LinkedList >> collect: collectBlock thenReject: rejectBlock [
	"Optimized version of SequenceableCollection>>#collect:#thenReject:"

	| newCollection newElement |
	newCollection := self class new.
	self
		do: [ :each |
			newElement := collectBlock value: each.
			(rejectBlock value: newElement)
				ifFalse: [ newCollection add: newElement ] ].
	^ newCollection
]

{ #category : #enumerating }
LinkedList >> collect: collectBlock thenSelect: selectBlock [
	"Optimized version of SequenceableCollection>>#collect:#thenSelect:"

	| newCollection newElement |
	newCollection := self class new.
	self
		do: [ :each |
			newElement := collectBlock value: each.
			(selectBlock value: newElement)
				ifTrue: [ newCollection add: newElement ] ].
	^ newCollection
]

{ #category : #copying }
LinkedList >> copyWith: newElement [
	^self copy add: newElement; yourself
]

{ #category : #copying }
LinkedList >> copyWithout: oldElement [
	|newInst|
	newInst := self class new.
	self do: [:each | each = oldElement ifFalse: [newInst add: each]].
	^newInst
]

{ #category : #enumerating }
LinkedList >> do: aBlock [

	| aLink |
	aLink := firstLink.
	[aLink == nil] whileFalse:
		[aBlock value: aLink value.
		 aLink := aLink nextLink]
]

{ #category : #accessing }
LinkedList >> first [
	"Answer the first link. Create an error notification if the receiver is
	empty."
^self firstLink value
]

{ #category : #accessing }
LinkedList >> firstLink [
	"Answer the first link. Create an error notification if the receiver is
	empty."

	self emptyCheck.
	^firstLink
]

{ #category : #accessing }
LinkedList >> indexOf: anElement startingAt: start ifAbsent: exceptionBlock [
	"Answer the index of the first occurrence of anElement after start
	within the receiver. If the receiver does not contain anElement,
	answer the 	result of evaluating the argument, exceptionBlock."

	|currentLink index|
	currentLink := self linkAt: start ifAbsent: [nil].
	index := start.
	[currentLink isNil ]
		whileFalse: [currentLink value = anElement value ifTrue: [^index].
					currentLink := currentLink nextLink.
					index := index +1].
	^exceptionBlock value
]

{ #category : #testing }
LinkedList >> isEmpty [

	^firstLink isNil
]

{ #category : #accessing }
LinkedList >> last [
	"Answer the last link. Create an error notification if the receiver is
	empty."


	^self lastLink value
]

{ #category : #accessing }
LinkedList >> lastLink [
	"Answer the last link. Create an error notification if the receiver is
	empty."

	self emptyCheck.
	^lastLink
]

{ #category : #private }
LinkedList >> linkAt: index [

	^self linkAt: index ifAbsent: [ self errorSubscriptBounds: index]
]

{ #category : #private }
LinkedList >> linkAt: index ifAbsent: errorBlock [

	| i |
	i := 0.
	self linksDo: [:link |
		(i := i + 1) = index ifTrue: [^ link]].
	^ errorBlock value
]

{ #category : #private }
LinkedList >> linkOf: anObject [
	^ self
		linkOf: anObject
		ifAbsent: [self error: 'No such element']
]

{ #category : #private }
LinkedList >> linkOf: anObject ifAbsent: errorBlock [

	self
		linksDo: [:el | el value = anObject
				ifTrue: [^ el]].
	^ errorBlock value
]

{ #category : #enumerating }
LinkedList >> linksDo: aBlock [

	| aLink |
	aLink := firstLink.
	[aLink == nil ] whileFalse:
		[
		aBlock value: aLink.
		aLink := aLink nextLink]
]

{ #category : #copying }
LinkedList >> postCopy [
	| aLink |
	super postCopy.
	firstLink ifNotNil: [
		aLink := firstLink := firstLink copy.
		[aLink nextLink isNil] whileFalse: [aLink nextLink: (aLink := aLink nextLink copy)].
		lastLink := aLink]
]

{ #category : #enumerating }
LinkedList >> reject: rejectBlock [
	"Optimized version of SequenceableCollection>>#reject:"

	| newCollection |
	newCollection := self class new.
	self
		do: [ :each |
			(rejectBlock value: each)
				ifFalse: [ newCollection add: each ] ].
	^ newCollection
]

{ #category : #enumerating }
LinkedList >> reject: rejectBlock thenCollect: collectBlock [
	"Optimized version of SequenceableCollection>>#reject:thenCollect:"

	| newCollection |
	newCollection := self class new.
	self
		do: [ :each |
			(rejectBlock value: each)
				ifFalse: [ newCollection add: (collectBlock value: each) ] ].
	^ newCollection
]

{ #category : #removing }
LinkedList >> remove: aLinkOrObject ifAbsent: aBlock [
	"Remove aLink from the receiver. If it is not there, answer the result of evaluating aBlock."

	| link |
	link := self linkOf: aLinkOrObject ifAbsent: [^aBlock value].
	self removeLink: link ifAbsent: [^aBlock value].
	^aLinkOrObject
]

{ #category : #removing }
LinkedList >> removeAll [
	"Implementation note: this has to be fast"

	firstLink := lastLink := nil
]

{ #category : #removing }
LinkedList >> removeAllSuchThat: aBlock [
	"Evaluate aBlock for each element and remove all that elements from
	the receiver for that aBlock evaluates to true.  For LinkedLists, it's safe to use do:."

	self do: [:each | (aBlock value: each) ifTrue: [self remove: each]]
]

{ #category : #removing }
LinkedList >> removeFirst [
	"Remove the first element and answer it. If the receiver is empty, create
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [firstLink := oldLink nextLink].
	oldLink nextLink: nil.
	^oldLink value
]

{ #category : #removing }
LinkedList >> removeLast [
	"Remove the receiver's last element and answer it. If the receiver is
	empty, create an error notification."

	| oldLink aLink |
	self emptyCheck.
	oldLink := lastLink.
	firstLink == lastLink
		ifTrue: [firstLink := nil. lastLink := nil]
		ifFalse: [aLink := firstLink.
				[aLink nextLink == oldLink] whileFalse:
					[aLink := aLink nextLink].
				 aLink nextLink: nil.
				 lastLink := aLink].
	oldLink nextLink: nil.
	^oldLink value
]

{ #category : #removing }
LinkedList >> removeLink: aLink [
	^self removeLink: aLink ifAbsent: [self error: 'no such method!']
]

{ #category : #removing }
LinkedList >> removeLink: aLink ifAbsent: aBlock [
	"Remove aLink from the receiver. If it is not there, answer the result of
	evaluating aBlock."

	| tempLink |
	aLink == firstLink
		ifTrue: [firstLink := aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := nil]]
		ifFalse: [tempLink := firstLink.
				[tempLink == nil ifTrue: [^aBlock value].
				 tempLink nextLink == aLink]
					whileFalse: [tempLink := tempLink nextLink].
				tempLink nextLink: aLink nextLink.
				aLink == lastLink
					ifTrue: [lastLink := tempLink]].
	"Not nilling the link enables us to delete while iterating"
	"aLink nextLink: nil."
	^aLink
]

{ #category : #enumerating }
LinkedList >> select: aBlock [
	"Reimplemennt #select: for speedup on linked lists.
	The super implemention accesses the linkes by index, thus causing an O(n^2)"

	| newCollection |
	newCollection := self class new.
	self do: [ :each |
		(aBlock value: each)
			ifTrue: [ newCollection add: each ]].
	^newCollection
]

{ #category : #enumerating }
LinkedList >> select: selectBlock thenCollect: collectBlock [
	"Optimized version of SequenceableCollection>>#select:thenCollect:"

	| newCollection |
	newCollection := self class new.
	self
		do: [ :each |
			(selectBlock value: each)
				ifTrue: [ newCollection add: (collectBlock value: each) ] ].
	^ newCollection
]

{ #category : #enumerating }
LinkedList >> select: selectBlock thenDo: doBlock [
	"Optimized version of SequenceableCollection>>#select:thenDo:"

	self
		do: [ :each |
				(selectBlock value: each)
					ifTrue: [ doBlock value: each ] ]
]

{ #category : #accessing }
LinkedList >> size [
	"Answer how many elements the receiver contains."

	| tally |
	tally := 0.
	self do: [:each | tally := tally + 1].
	^ tally
]

{ #category : #sorting }
LinkedList >> sort: aBlock [
	|aLinkedList|
	self isEmpty ifTrue: [ ^self ].
	aLinkedList := LinkedList newFrom: (self asArray sort: aBlock).
	firstLink := aLinkedList firstLink.
	lastLink := aLinkedList lastLink
]

{ #category : #private }
LinkedList >> species [

	^ Array
]

{ #category : #accessing }
LinkedList >> swap: ix1 with: ix2 [
	"Reimplemented, super would create an infinite loop"
	| minIx maxIx link1Prev link2Prev link1 link2 link1Next link2Next newLink2Next |
	((self validIndex: ix1) and: [self validIndex: ix2])	ifFalse: [^ self errorOutOfBounds].

	"Get edge case out of the way"
	ix1 = ix2 ifTrue: [^ self ].

	"Sort indexes to make boundary-checks easier"
	minIx := ix1 min: ix2.
	maxIx := ix2 max: ix1.

	link1Prev := (minIx = 1) ifFalse: [self linkAt: minIx -1].
	link1 := link1Prev ifNotNil: [ link1Prev nextLink]
				ifNil: [self linkAt: minIx].
	link1Next := link1 nextLink.
	link2Prev := self linkAt: maxIx -1.
	link2 := link2Prev nextLink.
	link2Next := link2 nextLink.

	"Link at start being swapped"
	link1 = firstLink ifTrue: [firstLink := link2.] ifFalse: [link1Prev nextLink: link2].
	"Link at end being swapped"
	link2 = lastLink ifTrue: [lastLink := link1] ifFalse: [].
	"Links  being swapped adjacent"
	newLink2Next := (link1 nextLink = link2) ifTrue: [link1] ifFalse: [link2Prev nextLink: link1.
		link1Next].
	link1 nextLink: link2Next.
	link2 nextLink: newLink2Next
]

{ #category : #private }
LinkedList >> validIndex: index [
	 ^index > 0
			and: [index <= self size]
]
