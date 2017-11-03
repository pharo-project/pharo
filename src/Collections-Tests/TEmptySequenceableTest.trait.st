"
A trait for test purposes
"
Trait {
	#name : #TEmptySequenceableTest,
	#category : #Collections-Tests-Abstract
}

{ #category : #parameters }
TEmptySequenceableTest >> accessValuePutIn [
	"return access the element put in the non-empty collection"
	
	^ self perform: self selectorToAccessValuePutIn
]

{ #category : #parameters }
TEmptySequenceableTest >> accessValuePutInOn: s [

	"return access the element put in the non-empty collection"
	
	^ s perform: self selectorToAccessValuePutIn
]

{ #category : #requirements }
TEmptySequenceableTest >> empty [
	
	^ self explicitRequirement
]

{ #category : #requirements }
TEmptySequenceableTest >> nonEmpty [
	
	^ self explicitRequirement
]

{ #category : #requirements }
TEmptySequenceableTest >> selectorToAccessValuePutIn [
	"return the selector of the method that should be invoked to access an element"
	
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TEmptySequenceableTest >> test0FixtureEmptySequenceableTest [
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self empty.
	self assert: self empty isEmpty
]

{ #category : #'tests - sequence isempty' }
TEmptySequenceableTest >> testSequenceIfNotEmpty [

	self assert: (self nonEmpty 
					ifNotEmpty: [:s | self accessValuePutInOn: s]) = self valuePutIn
]

{ #category : #'tests - sequence isempty' }
TEmptySequenceableTest >> testSequenceIfNotEmptyifEmpty [

	self assert: (self nonEmpty ifEmpty: [false] ifNotEmpty: [:s | (self accessValuePutInOn: s) = self valuePutIn])
]

{ #category : #parameters }
TEmptySequenceableTest >> valuePutIn [
	"the value that we will put in the non empty collection"
	
	^ #x
]
