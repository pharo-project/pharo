Class {
	#name : 'AbstractTraitsOnPreparedModelTest',
	#superclass : 'TraitAbstractTest',
	#instVars : [
		't1',
		't2',
		't3',
		't4',
		't5',
		't6',
		't8',
		't9',
		't10',
		't11',
		'c1',
		'c2',
		'c3',
		'c4',
		'c5',
		'c6',
		'c7',
		'c8',
		'c9',
		'c10',
		'c11',
		't12'
	],
	#category : 'Traits-Tests',
	#package : 'Traits-Tests'
}

{ #category : 'utilities' }
AbstractTraitsOnPreparedModelTest >> assertPrints: aString like: anotherString [
	self assert: (aString copyWithout: $ ) equals: (anotherString copyWithout: $ )
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c1 [
	^c1
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c10 [
	^c10
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c10: anObject [
	^c10 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c11 [
	^c11
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c11: anObject [
	^c11 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c1: anObject [
	^c1 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c2 [
	^c2
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c2: anObject [
	^c2 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c3 [
	^c3
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c3: anObject [
	^c3 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c4 [
	^c4
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c4: anObject [
	^c4 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c5 [
	^c5
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c5: anObject [
	^c5 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c6 [
	^c6
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c6: anObject [
	^c6 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c7 [
	^c7
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c7: anObject [
	^c7 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c8 [
	^c8
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c8: anObject [
	^c8 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c9 [
	^c9
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> c9: anObject [
	^c9 := anObject
]

{ #category : 'running' }
AbstractTraitsOnPreparedModelTest >> setUp [
	"Please note, that most tests rely on this setup of traits and
	classes - and that especially the order of the definitions matters."
	"SetUpCount := SetUpCount + 1."

	super setUp.
	SystemAnnouncer uniqueInstance suspendAllWhile:
			[self t1: (self newTrait: #T1
						traits: { }).
			self t1 comment: 'I am the trait T1'.

			self t1 compile: 'm11 ^11' classified: #cat1.
			self t1 compile: 'm12 ^12' classified: #cat2.
			self t1 compile: 'm13 ^self m12' classified: #cat3.

			self t2: (self newTrait: #T2
						traits: { }).
			self t2 compile: 'm21 ^21' classified: #cat1.
			self t2 compile: 'm22 ^22' classified: #cat2.
			self t2 classSide compile: 'm2ClassSide: a ^a'.

			self t3: (self newTrait: #T3
						traits: { }).
			self t3 compile: 'm31 ^31' classified: #cat1.
			self t3 compile: 'm32 ^32' classified: #cat2.
			self t3 compile: 'm33 ^33' classified: #cat3.

			self t4: (self newTrait: #T4
						traits: { (self t1). (self t2) }).
			self t4 compile: 'm11 ^41' classified: #catX.	"overrides T1>>m11"
			self t4 compile: 'm42 ^42' classified: #cat2.

			self t5: (self newTrait: #T5 traits: self t1 + self t2).
			self t5 compile: 'm51 ^super foo' classified: #cat1.
			self t5 compile: 'm52 ^ self class bar' classified: #cat1.
			self t5 compile: 'm53 ^ self class bar' classified: #cat1.

			self t6: (self newTrait: #T6
						traits: self t1 + (self t2 @ { (#m22Alias -> #m22) })).

			self t9: (self newTrait: #T9
						traits: { }).

			self t10: (self newTrait: #T10
						traits: { }).

			self t12: (self newTrait: #T12
						traits: {}).

			self t11: (self newTrait: #T11
						traits: { self t12}).



			self c1: (self
						newClass: #C1
						superclass: Object
						traits: { }).
			self c1 compile: 'foo ^true' classified: #accessing.
			self c2: (self
						newClass: #C2
						superclass: self c1
						traits: self t5 - { #m11 }).
			self c2 compile: 'foo ^false' classified: #private.
			self c2 compile: 'bar ^self foo' classified: #private.

			self c9: (self
						newClass: #C9
						superclass: Object
						traits: self t9 ).

			self c10: (self
						newClass: #C10
						superclass: self c9
						traits: self t10 ).

			self c11: (self
						newClass: #C11
						superclass: self c10
						traits: self t11 ).

			self setUpTrivialRequiresFixture.
			self setUpTwoLevelRequiresFixture.
			self setUpTranslatingRequiresFixture]
]

{ #category : 'running' }
AbstractTraitsOnPreparedModelTest >> setUpTranslatingRequiresFixture [
	self c6: (self
				newClass: #C6
				superclass: ProtoObject
				traits: { }).
	ProtoObject removeSubclass: self c6.
	self c6 superclass: nil.
	self c7: (self
				newClass: #C7
				superclass: self c6
				traits: { }).
	self c8: (self
				newClass: #C8
				superclass: self c7
				traits: { }).
	self c6 compile: 'foo ^self x' classified: #accessing.
	self c7 compile: 'foo ^3' classified: #accessing.
	self c7 compile: 'bar ^super foo' classified: #accessing.
	self c8 compile: 'bar ^self blah' classified: #accessing
]

{ #category : 'running' }
AbstractTraitsOnPreparedModelTest >> setUpTrivialRequiresFixture [
	self c3: (self
				newClass: #C3
				superclass: ProtoObject
				traits: { }).
	ProtoObject removeSubclass: self c3.
	self c3 superclass: nil.
	self c3 compile: 'foo ^self bla' classified: #accessing
]

{ #category : 'running' }
AbstractTraitsOnPreparedModelTest >> setUpTwoLevelRequiresFixture [
	self c4: (self
				newClass: #C4
				superclass: ProtoObject
				traits: { }).
	ProtoObject removeSubclass: self c4.
	self c4 superclass: nil.
	self c5: (self
				newClass: #C5
				superclass: self c4
				traits: { }).
	self c4 compile: 'foo ^self blew' classified: #accessing.
	self c5 compile: 'foo ^self blah' classified: #accessing
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t1 [
	^t1
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t10 [
	^t10
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t10: anObject [
	^t10 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t11 [
	^t11
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t11: anObject [
	^t11 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t12 [
	^t12
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t12: anObject [
	^t12 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t1: anObject [
	^t1 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t2 [
	^t2
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t2: anObject [
	^t2 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t3 [
	^t3
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t3: anObject [
	^t3 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t4 [
	^t4
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t4: anObject [
	^t4 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t5 [
	^t5
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t5: anObject [
	^t5 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t6 [
	^t6
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t6: anObject [
	^t6 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t8 [
	^t8
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t8: anObject [
	^t8 := anObject
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t9 [
	^t9
]

{ #category : 'accessing' }
AbstractTraitsOnPreparedModelTest >> t9: anObject [
	^t9 := anObject
]
