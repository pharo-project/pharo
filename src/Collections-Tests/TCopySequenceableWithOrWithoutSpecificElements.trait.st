"
A trait for test purposes
"
Trait {
	#name : #TCopySequenceableWithOrWithoutSpecificElements,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TCopySequenceableWithOrWithoutSpecificElements >> indexInNonEmpty [
" return an index between bounds of 'nonEmpty' "

	self explicitRequirement
]

{ #category : #requirements }
TCopySequenceableWithOrWithoutSpecificElements >> nonEmpty [
	self explicitRequirement
]

{ #category : #'tests - fixture' }
TCopySequenceableWithOrWithoutSpecificElements >> test0FixtureCopyWithOrWithoutSpecificElementsTest [

	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self indexInNonEmpty.
	self assert: self indexInNonEmpty > 0.
	self assert: self indexInNonEmpty <= self nonEmpty size
]

{ #category : #'tests - copying with or without' }
TCopySequenceableWithOrWithoutSpecificElements >> testCopyWithFirst [

	| index element result |
	index:= self indexInNonEmpty .
	element:= self nonEmpty at: index.
	
	result := self nonEmpty copyWithFirst: element.	
	
	self assert: result size = (self nonEmpty size + 1).
	self assert: result first = element .
	
	2 to: result size do:
	[ :i |
	self assert: (result at: i) = ( self nonEmpty at: ( i - 1 ))].
]

{ #category : #'tests - copying with or without' }
TCopySequenceableWithOrWithoutSpecificElements >> testCopyWithSequenceable [

	| result index element |
	index := self indexInNonEmpty.
	element := self nonEmpty at: index.
	result := self nonEmpty copyWith: (element ).
	
	self assert: result size equals: (self nonEmpty size + 1).
	self assert: result last equals: element.
	
	1 to: (result size - 1) do:
		[ :i | self assert: (result at: i) equals: ( self nonEmpty at: i)]
]

{ #category : #'tests - copying with or without' }
TCopySequenceableWithOrWithoutSpecificElements >> testCopyWithoutFirst [

	| result |
	result := self nonEmpty copyWithoutFirst.
	
	self assert: result size equals: (self nonEmpty size - 1).
	
	result withIndexDo: 
		[:el :i | self assert: (result at: i) equals: (self nonEmpty at: (i + 1))]
]

{ #category : #'tests - copying with or without' }
TCopySequenceableWithOrWithoutSpecificElements >> testCopyWithoutIndex [

	| result index |	
	index := self indexInNonEmpty.
	result := self nonEmpty copyWithoutIndex: index.
	
	"Verify content of result"
	result withIndexDo: 
		[:element :i | 
		 i <  index ifTrue: [ self assert: element equals: (self nonEmpty at: i) ].
		 i >= index ifTrue: [ self assert: element equals: (self nonEmpty at: (i + 1)) ]].
	
	"Verify size of result"
	self assert: result size equals: (self nonEmpty size - 1)
]

{ #category : #'tests - copying with or without' }
TCopySequenceableWithOrWithoutSpecificElements >> testForceToPaddingStartWith [

	| result element |
	element := self nonEmpty at: self indexInNonEmpty .
	result := self nonEmpty forceTo: (self nonEmpty size+2) paddingStartWith: ( element ).
	
	"verify content of 'result' : "
	1 to: 2   do:
		[:i | self assert: ( element ) = ( result at:(i) ) ].
	
	3 to: result size do:
		[:i | self assert: ( result at:i ) = ( self nonEmpty at:(i-2) ) ].

	"verify size of 'result' :"
	self assert: result size = (self nonEmpty size + 2).
]

{ #category : #'tests - copying with or without' }
TCopySequenceableWithOrWithoutSpecificElements >> testForceToPaddingWith [

	| result element |
	element := self nonEmpty at: self indexInNonEmpty .
	result := self nonEmpty forceTo: (self nonEmpty size+2) paddingWith: ( element ).
	
	"verify content of 'result' : "
	1 to: self nonEmpty  size do:
		[:i | self assert: ( self nonEmpty at: i ) = ( result at:(i) ). ].
	
	(result size - 1) to: result size do:
		[:i | self assert: ( result at:i ) = ( element ) ].

	"verify size of 'result' :"
	self assert: result size = (self nonEmpty size + 2).
]
