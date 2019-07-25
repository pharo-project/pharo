"
I am designed specifically for collections.
"
Class {
	#name : #CollectionValueHolder,
	#superclass : #NewValueHolder,
	#category : #'NewValueHolder-Core-Base'
}

{ #category : #protocol }
CollectionValueHolder >> add: newObject [
	| result |
	result := value add: newObject.
	self valueAdded: newObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> add: newObject after: oldObject [ 
	
	| result |
	result := value add: newObject after: oldObject.
	self valueAdded: newObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> add: newObject afterIndex: index [ 
	
	| result |
	result := value add: newObject afterIndex: index.
	self valueAdded: newObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> add: newObject before: oldObject [
	
	| result |
	result := value add: newObject before: oldObject.
	self valueAdded: newObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> add: newObject beforeIndex: index [ 
	
	| result |
	result := value add: newObject beforeIndex: index.
	self valueAdded: newObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> addAll: aCollection [ 
	
	| result |
	result := value addAll: aCollection.
	self valueAdded: aCollection.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> addAllFirst: anOrderedCollection [
	
	| result |
	result := value addAllFirst: anOrderedCollection.
	self valueAdded: anOrderedCollection.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> addAllFirstUnlessAlreadyPresent: anOrderedCollection [
	
	| result |
	result := value addAllFirstUnlessAlreadyPresent: anOrderedCollection.
	self valueAdded: anOrderedCollection.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> addAllLast: aCollection [ 
	
	| result |
	result := value addAllLast: aCollection.
	self valueAdded: aCollection.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> addFirst: newObject [ 

	| result |
	result := value addFirst: newObject .
	self valueAdded: newObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> addLast: newObject [ 

	| result |
	result := value addLast: newObject .
	self valueAdded: newObject.
	^ result
]

{ #category : #override }
CollectionValueHolder >> at: anObject [

	^ value at: anObject
]

{ #category : #protocol }
CollectionValueHolder >> at: index ifAbsentPut: block [

	| result |
	result := value at: index ifAbsentPut: block.
	self valueAdded: block value.
	^ result
]

{ #category : #override }
CollectionValueHolder >> at: key put: anObject [

	value at: key put: anObject.
	self valueAdded: anObject.
	^ anObject
]

{ #category : #protocol }
CollectionValueHolder >> atAll: aCollection put: anObject [ 
	
	| result |
	result := value atAll: aCollection put: anObject .
	self valueAdded: anObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> atAll: indexArray putAll: valueArray [
	
	| result |
	result := value atAll: indexArray putAll: valueArray.
	self valueAdded: valueArray.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> atAllPut: anObject [ 
	
	| result |
	result := value atAllPut: anObject.
	self valueAdded: anObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> atLast: indexFromEnd put: obj [
	
	| result |
	result := value atLast: indexFromEnd put: obj.
	self valueAdded: obj.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> atWrap: index put: anObject [
	
	| result |
	result := value atWrap: index put: anObject.
	self valueAdded: value.
	^ result
]

{ #category : #private }
CollectionValueHolder >> doesNotUnderstand: aMessage [

	^ (value respondsTo: aMessage selector)
		ifTrue: [ value perform: aMessage selector withEnoughArguments: aMessage arguments ]
		ifFalse: [ super doesNotUnderstand: aMessage ]
]

{ #category : #protocol }
CollectionValueHolder >> insert: anObject before: spot [

	| result |
	result := value insert: anObject before: spot.
	self valueAdded: anObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> remove: oldObject [

	| result |
	result := value remove: oldObject.
	self valueRemoved: oldObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> remove: oldObject ifAbsent: absentBlock [

	| result |
	result := value remove: oldObject ifAbsent: absentBlock.
	self valueRemoved: oldObject.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> removeAll [
	"Note: I'm not sure about the semantics here, because Collections normally don't return anything on #removeAll, even though one might expect the original content. Use with care."

	value removeAll.
	self valueRemoved: nil
]

{ #category : #protocol }
CollectionValueHolder >> removeAll: aCollection [
	| result |
	result := value removeAll: aCollection.
	self valueRemoved: result.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> removeAt: index [

	| result |
	result := value removeAt: index.
	self valueRemoved: result.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> removeFirst [

	| result |
	result := value removeFirst.
	self valueRemoved: result.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> removeFirst: n [ 

	| result |
	result := value removeFirst: n .
	self valueRemoved: result.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> removeIndex: removedIndex [

	| result |
	result := value removeIndex: removedIndex.
	self valueRemoved: result.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> removeLast [

	| result |
	result := value removeLast.
	self valueRemoved: result.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> removeLast: n [

	| result |
	result := value removeLast: n.
	self valueRemoved: result.
	^ result
]

{ #category : #protocol }
CollectionValueHolder >> replace: aBlock [
	value replace: aBlock.
	self valueChanged
]

{ #category : #protocol }
CollectionValueHolder >> replaceAll: oldObject with: newObject [
	value replaceAll: oldObject with: newObject.
	self valueChanged: oldObject
]

{ #category : #initialization }
CollectionValueHolder >> reset [

	value reset.
	self valueChanged
]

{ #category : #protocol }
CollectionValueHolder >> resetTo: index [

	value resetTo: index.
	self valueChanged
]

{ #category : #override }
CollectionValueHolder >> size [ 

	^ value size
]

{ #category : #protocol }
CollectionValueHolder >> sort: aSortBlock [ 

	value sort: aSortBlock .
	self valueChanged
]

{ #category : #announcing }
CollectionValueHolder >> valueAdded: newValue [
	self announcer announce: (ValueAdded newValue: newValue).
	self valueChanged: newValue
]

{ #category : #announcing }
CollectionValueHolder >> valueRemoved: oldValue [
	self announcer announce: (ValueRemoved oldValue: oldValue).
	self valueChanged: oldValue
]

{ #category : #announcements }
CollectionValueHolder >> whenAddedDo: aBlock [
	"Culled block [ :newValue :announcement | ]"

	self announcer when: ValueAdded do: [ :ann | aBlock cull: ann newValue cull: ann ]
]

{ #category : #announcements }
CollectionValueHolder >> whenRemovedDo: aBlock [
	"Culled block [ :oldValue :announcement | ]"

	self announcer when: ValueRemoved do: [ :ann | aBlock cull: ann oldValue cull: ann ]
]
