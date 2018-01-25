"
A trait for test purposes
"
Trait {
	#name : #TPutTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TPutTest >> aValue [
" return a value to put into nonEmpty"
	^ self explicitRequirement
]

{ #category : #requirements }
TPutTest >> indexArray [
" return a Collection including indexes between bounds of 'nonEmpty' "

	self explicitRequirement.
]

{ #category : #requirements }
TPutTest >> nonEmpty [

	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TPutTest >> test0FixturePutOneOrMoreElementsTest [
	self aValue.

	
	self indexArray.
	self indexArray do: [
		:each| 
		self assert: each class = SmallInteger. 
		self assert: (each>=1 & each<= self nonEmpty size).
		].
	
	self assert: self indexArray size = self valueArray size.
	
	self empty.
	self assert: self empty isEmpty .
	
	self nonEmpty.
	self deny: self nonEmpty  isEmpty.
]

{ #category : #'tests - puting with indexes' }
TPutTest >> testAtAllIndexesPut [
	
	self nonEmpty atAllPut: self aValue.
	self nonEmpty do:[ :each| self assert: each = self aValue].
	
]

{ #category : #'tests - puting with indexes' }
TPutTest >> testAtAllPut [
	| |
	self nonEmpty atAll: self indexArray put: self aValue..
	
	self indexArray do:
		[:i | self assert: (self nonEmpty at: i)=self aValue ].
	
]

{ #category : #'tests - puting with indexes' }
TPutTest >> testAtAllPutAll [

	| aValueArray |
	aValueArray := self valueArray .
	self nonEmpty atAll: self indexArray putAll: aValueArray  .
	
	1 to: self indexArray size do:
		[:i |
		self assert: (self nonEmpty at:(self indexArray at: i))= (aValueArray  at:i) ]
]

{ #category : #'tests - puting with indexes' }
TPutTest >> testAtLastPut [
	| result index |
	index := self indexArray anyOne.
	result := self nonEmpty atLast: index  put: self aValue.
	
	self assert: (self nonEmpty at: (self nonEmpty size +1 - index)) = self aValue .
]

{ #category : #'tests - puting with indexes' }
TPutTest >> testAtWrapPut [
	 
	| index |
	index := self indexArray anyOne.
	
	self nonEmpty atWrap: 0 put: self aValue.
	self assert: (self nonEmpty at:(self nonEmpty size))=self aValue.
	
	self nonEmpty atWrap: (self nonEmpty size+1) put: self aValue.
	self assert: (self nonEmpty at:(1))=self aValue.
	
	self nonEmpty atWrap: (index  ) put: self aValue.
	self assert: (self nonEmpty at: index ) = self aValue.
	
	self nonEmpty atWrap: (self nonEmpty size+index  ) put: self aValue .
	self assert: (self nonEmpty at:(index ))=self aValue .
]

{ #category : #'tests - puting with indexes' }
TPutTest >> testFromToPut [
	
	| collection index |
	index := self indexArray anyOne.
	collection := self nonEmpty copy.
	collection from: 1 to: index  put: self aValue..
	1 to: index do:
		[:i | self assert: (collection at: i)= self aValue].
	(index +1) to: collection size do:
		[:i | self assert: (collection at:i)= (self nonEmpty at:i)].
]

{ #category : #'tests - puting with indexes' }
TPutTest >> testSwapWith [
 
	| result index |
	index := self indexArray anyOne.
	result:= self nonEmpty copy .
	result swap: index with: 1.
	self assert: (result at: index) = (self nonEmpty at:1).
	self assert: (result at: 1) = (self nonEmpty at: index).
	
]

{ #category : #requirements }
TPutTest >> valueArray [
" return a collection (with the same size than 'indexArray' )of values to be put in 'nonEmpty'  at indexes in 'indexArray' "
	| result |
	result := Array new: self indexArray size.
	1 to: result size do:
		[:i |
		result at:i put: (self aValue ).
		].
	^ result.
]
