"
SUnit tests for reducing collections
"
Class {
	#name : #ReduceTest,
	#superclass : #TestCase,
	#category : #'Collections-Tests-Sequenceable'
}

{ #category : #accessing }
ReduceTest >> collectionClasses [
	^ (Array with: Set with: Bag) , (self sequenceableClasses)
]

{ #category : #accessing }
ReduceTest >> sequenceableClasses [
	^ Array with: Array with: OrderedCollection with: SortedCollection
]

{ #category : #tests }
ReduceTest >> testReduceCollection2Arguments [
	| block |
	block := [ :a :b | a + b ].
	self collectionClasses
		do: [ :class | 
			self should: [ class new reduce: block ] raise: Error.
			self assert: ((class with: 1) reduce: block) equals: 1.
			self assert: ((class with: 1 with: 2) reduce: block) equals: 3.
			self assert: ((class with: 1 with: 2 with: 3) reduce: block) equals: 6.
			self
				assert:
					((class
						with: 1
						with: 2
						with: 3
						with: 4) reduce: block)
				equals: 10 ]
]

{ #category : #tests }
ReduceTest >> testReduceCollection3Arguments [
	| block |
	block := [ :a :b :c | a + b + c ].
	self collectionClasses
		do: [ :class | 
			self should: [ class new reduce: block ] raise: Error.
			self assert: ((class with: 1) reduce: block) equals: 1.
			self should: [ (class with: 1 with: 2) reduce: block ] raise: Error.
			self assert: ((class with: 1 with: 2 with: 3) reduce: block) equals: 6.
			self
				should: [ (class
						with: 1
						with: 2
						with: 3
						with: 4) reduce: block ]
				raise: Error ]
]

{ #category : #tests }
ReduceTest >> testReduceLeft2Arguments [
	| block |
	block := [ :a :b | Array with: a with: b ].
	self sequenceableClasses
		do: [ :class | 
			self should: [ class new reduceLeft: block ] raise: Error.
			self assert: ((class with: #a) reduceLeft: block) equals: #a.
			self assert: ((class with: #a with: #b) reduceLeft: block) equals: #(a b).
			self assert: ((class with: #a with: #b with: #c) reduceLeft: block) equals: #(#(a b) c).
			self
				assert:
					((class
						with: #a
						with: #b
						with: #c
						with: #d) reduceLeft: block)
				equals: #(#(#(a b) c) d).
			self
				assert:
					((class
						with: #a
						with: #b
						with: #c
						with: #d
						with: #e) reduceLeft: block)
				equals: #(#(#(#(a b) c) d) e) ]
]

{ #category : #tests }
ReduceTest >> testReduceLeft3Arguments [
	| block |
	block := [ :a :b :c | Array with: a with: b with: c ].
	self sequenceableClasses
		do: [ :class | 
			self should: [ class new reduceLeft: block ] raise: Error.
			self assert: ((class with: #a) reduceLeft: block) equals: #a.
			self should: [ (class with: #a with: #b) reduceLeft: block ] raise: Error.
			self assert: ((class with: #a with: #b with: #c) reduceLeft: block) equals: #(a b c).
			self
				should: [ (class
						with: #a
						with: #b
						with: #c
						with: #d) reduceLeft: block ]
				raise: Error.
			self
				assert:
					((class
						with: #a
						with: #b
						with: #c
						with: #d
						with: #e) reduceLeft: block)
				equals: #(#(a b c) d e) ]
]

{ #category : #tests }
ReduceTest >> testReduceLeftSpecial [
	self assert: ((1 to: 100) reduceLeft: [ :a :b | a - b ]) equals: -5048.
	self assert: ('abc' reduceLeft: [ :a :b | Array with: a with: b ]) equals: #(#($a $b) $c)
]

{ #category : #tests }
ReduceTest >> testReduceRight2Arguments [
	| block |
	block := [ :a :b | Array with: a with: b ].
	self sequenceableClasses
		do: [ :class | 
			self should: [ class new reduceRight: block ] raise: Error.
			self assert: ((class with: #a) reduceRight: block) equals: #a.
			self assert: ((class with: #a with: #b) reduceRight: block) equals: #(a b).
			self assert: ((class with: #a with: #b with: #c) reduceRight: block) equals: #(a #(b c)).
			self
				assert:
					((class
						with: #a
						with: #b
						with: #c
						with: #d) reduceRight: block)
				equals: #(a #(b #(c d))).
			self
				assert:
					((class
						with: #a
						with: #b
						with: #c
						with: #d
						with: #e) reduceRight: block)
				equals: #(a #(b #(c #(d e)))) ]
]

{ #category : #tests }
ReduceTest >> testReduceRight3Arguments [
	| block |
	block := [ :a :b :c | Array with: a with: b with: c ].
	self sequenceableClasses
		do: [ :class | 
			self should: [ class new reduceRight: block ] raise: Error.
			self assert: ((class with: #a) reduceRight: block) equals: #a.
			self should: [ (class with: #a with: #b) reduceRight: block ] raise: Error.
			self assert: ((class with: #a with: #b with: #c) reduceRight: block) equals: #(a b c).
			self
				should: [ (class
						with: #a
						with: #b
						with: #c
						with: #d) reduceRight: block ]
				raise: Error.
			self
				assert:
					((class
						with: #a
						with: #b
						with: #c
						with: #d
						with: #e) reduceRight: block)
				equals: #(a b #(c d e)) ]
]

{ #category : #tests }
ReduceTest >> testReduceRightSpecial [
	self assert: ((1 to: 100) reduceRight: [ :a :b | a - b ]) equals: -50.
	self assert: ('abc' reduceRight: [ :a :b | Array with: a with: b ]) equals: #($a #($b $c))
]
