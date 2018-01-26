"
A trait for test purposes
"
Trait {
	#name : #TIncludesTest,
	#category : #'Collections-Tests-Abstract'
}

{ #category : #requirements }
TIncludesTest >> anotherElementNotIn [
" return an element different of 'elementNotIn'  not included in 'nonEmpty' "
	^ self explicitRequirement
]

{ #category : #requirements }
TIncludesTest >> elementNotIn [
"return an element not included in 'nonEmpty' "

	^ self explicitRequirement
]

{ #category : #requirements }
TIncludesTest >> empty [
	^ self explicitRequirement
]

{ #category : #requirements }
TIncludesTest >> nonEmpty [
	^ self explicitRequirement
]

{ #category : #'tests - fixture' }
TIncludesTest >> test0FixtureIncludeTest [
	| anElementIn |
	self nonEmpty.
	self deny: self nonEmpty isEmpty.
	self elementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self elementNotIn ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = false.
	self anotherElementNotIn.
	anElementIn := true.
	self nonEmpty detect: [ :each | each = self anotherElementNotIn ] ifNone: [ anElementIn := false ].
	self assert: anElementIn = false.
	self empty.
	self assert: self empty isEmpty
]

{ #category : #'tests - includes' }
TIncludesTest >> testIdentityIncludesNonSpecificComportement [
	" test the same comportement than 'includes: '  "
	| collection |	
	collection := self nonEmpty  .
	
	self deny: (collection identityIncludes: self elementNotIn ).
	self assert:(collection identityIncludes: collection anyOne)

]

{ #category : #'tests - includes' }
TIncludesTest >> testIncludesAllNoneThere [

	self deny: (self empty includesAll: self nonEmpty ).
	self deny: (self nonEmpty includesAll: { self elementNotIn. self anotherElementNotIn })
]

{ #category : #'tests - includes' }
TIncludesTest >> testIncludesAnyAllThere [

	self deny: (self nonEmpty includesAny: self empty).
	self assert: (self nonEmpty includesAny: { self nonEmpty anyOne }).
	self assert: (self nonEmpty includesAny: self nonEmpty).
]

{ #category : #'tests - includes' }
TIncludesTest >> testIncludesAnyNoneThere [
	
	self deny: (self nonEmpty includesAny: self empty).
	self deny: (self nonEmpty includesAny: { self elementNotIn. self anotherElementNotIn })
]

{ #category : #'tests - includes' }
TIncludesTest >> testIncludesElementIsNotThere [
	
	self deny: (self nonEmpty includes: self elementNotIn).
	self assert: (self nonEmpty includes: self nonEmpty anyOne).
	self deny: (self empty includes: self elementNotIn)
]

{ #category : #'tests - includes' }
TIncludesTest >> testIncludesElementIsThere [
	
	self assert: (self nonEmpty includes: self nonEmpty anyOne).
]
