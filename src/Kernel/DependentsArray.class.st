"
Act as an array of (weak) dependents of some object.

When dependents are reclaimed, they are replaced by an UndefinedObject in the DependentsArray.
This is why instances of this class will take care to iterate only on non nil elements.
These nil also cause loops written as (1 to: self size do: [:i | (self at: i) doSomething]) to be inefficient.
This is because #size and #at: both require scanning for nils.
For this reason, DependentsArray though sequenceable, is not a subclass of SequenceableCollection.
"
Class {
	#name : #DependentsArray,
	#superclass : #Collection,
	#type : #weak,
	#category : #'Kernel-Models'
}

{ #category : #'instance creation' }
DependentsArray class >> with: anObject [
	^(self basicNew: 1) basicAt: 1 put: anObject; yourself
]

{ #category : #'instance creation' }
DependentsArray class >> with: firstObject with: secondObject [ 
	^(self basicNew: 2)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		yourself
]

{ #category : #'instance creation' }
DependentsArray class >> with: firstObject with: secondObject with: thirdObject [ 
	^(self basicNew: 3)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		basicAt: 3 put: thirdObject;
		yourself
]

{ #category : #'instance creation' }
DependentsArray class >> with: firstObject with: secondObject with: thirdObject with: fourthObject [ 
	^(self basicNew: 4)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		basicAt: 3 put: thirdObject;
		basicAt: 4 put: fourthObject;
		yourself
]

{ #category : #'instance creation' }
DependentsArray class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject [
	^(self basicNew: 5)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		basicAt: 3 put: thirdObject;
		basicAt: 4 put: fourthObject;
		basicAt: 5 put: fifthObject;
		yourself
]

{ #category : #'instance creation' }
DependentsArray class >> with: firstObject with: secondObject with: thirdObject with: fourthObject with: fifthObject with: sixthObject [
	^(self basicNew: 6)
		basicAt: 1 put: firstObject;
		basicAt: 2 put: secondObject;
		basicAt: 3 put: thirdObject;
		basicAt: 4 put: fourthObject;
		basicAt: 5 put: fifthObject;
		basicAt: 6 put: sixthObject;
		yourself
]

{ #category : #'instance creation' }
DependentsArray class >> withAll: aCollection [
	| newInstance |
	newInstance := self basicNew: aCollection size.
	1 to: aCollection size do: [:i |
		newInstance basicAt: i put: (aCollection at: i)].
	^newInstance
]

{ #category : #accessing }
DependentsArray >> at: anIndex [
	| basicSize counter dep |
	anIndex > 0 ifTrue: [
		basicSize := self basicSize.
		anIndex <= basicSize ifTrue: [
			counter := 0.
			1 to: basicSize do: [:i |
				(dep := self basicAt: i) == nil
					ifFalse: [(counter := counter + 1) = anIndex ifTrue: [^dep]]]]].
	self error: 'access with an index out of bounds'
]

{ #category : #accessing }
DependentsArray >> at: anIndex put: anObject [
	| basicSize counter |
	anIndex > 0 ifTrue: [
		basicSize := self basicSize.
		anIndex <= basicSize ifTrue: [
			counter := 0.
			1 to: basicSize do: [:i |
				(self basicAt: i) == nil
					ifFalse: [(counter := counter + 1) = anIndex ifTrue: [^self basicAt: i put: anObject]]]]].
	self error: 'access with an index out of bounds'
]

{ #category : #private }
DependentsArray >> basicReplaceFrom: start to: stop with: replacement startingAt: repStart [ 
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	start to: stop do: [:i |
		self basicAt: i put: (replacement basicAt: repStart - start + i)]
]

{ #category : #enumerating }
DependentsArray >> collect: aBlock [ 
	"Refer to the comment in Collection|select:."
	| basicSize newSelf size dep selection |
	basicSize := self basicSize.
	newSelf := self species new: basicSize.
	size := 0.
	1 to: basicSize do: [:i |
		(dep := self basicAt: i) notNil ifTrue: [newSelf basicAt: (size := size+1) put: (aBlock value: dep)]].
	selection := self species new: size.
	selection basicReplaceFrom: 1 to: size with: newSelf startingAt: 1.
	^selection
]

{ #category : #copying }
DependentsArray >> copyWith: newElement [ 
	"Re-implemented to not copy any niled out dependents."
	| copy i |
	copy := self class new: self size + 1.
	i := 0.
	self do: [:item | copy basicAt: (i:=i+1) put: item].
	copy basicAt: (i:=i+1) put: newElement.
	^copy
]

{ #category : #enumerating }
DependentsArray >> do: aBlock [
	"Evaluate a Block on non nil elements of the receiver"
	| dep |
	1 to: self basicSize do:[:i|
		(dep := self basicAt: i) ifNotNil:[aBlock value: dep]].
]

{ #category : #accessing }
DependentsArray >> first [
	self do: [:dep | ^dep].
	self error: 'this collection is empty'
]

{ #category : #accessing }
DependentsArray >> last [
	self reverseDo: [:dep | ^dep].
	self error: 'this collection is empty'
]

{ #category : #enumerating }
DependentsArray >> reverseDo: aBlock [
	"Refer to the comment in Collection|do:."
	| dep |
	self basicSize to: 1 by: -1 do: [:i |
		(dep := self basicAt: i) ifNotNil: [aBlock value: dep]]
]

{ #category : #enumerating }
DependentsArray >> select: aBlock [ 
	"Refer to the comment in Collection|select:."
	| basicSize newSelf size selection |
	basicSize := self basicSize.
	newSelf := self species new: basicSize.
	size := 0.
      1 to: basicSize do: [ :i |
               (self basicAt: i) ifNotNil: [ :dep |
                       (aBlock value: dep) ifTrue: [
                               newSelf basicAt: (size := size+1) put: dep ] ] ].
	selection := self species new: size.
	selection basicReplaceFrom: 1 to: size with: newSelf startingAt: 1.
	^selection
]

{ #category : #accessing }
DependentsArray >> size [
	"count each non nil elements in self.
	Note: count: will use do: which will already have filtered out nil elements"
	
	^self count: [:each | true]
]

{ #category : #converting }
DependentsArray >> writeStream [
	^ WriteStream on: self
]
