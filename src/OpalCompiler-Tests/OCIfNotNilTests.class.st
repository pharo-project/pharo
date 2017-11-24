Class {
	#name : #OCIfNotNilTests,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #tests }
OCIfNotNilTests >> testIfNilIfNotNil0Arg [

	self assert: (5 ifNil: [#foo] ifNotNil: [#bar]) = #bar.
	self assert: (nil ifNil: [#foo] ifNotNil: [#bar]) = #foo
]

{ #category : #tests }
OCIfNotNilTests >> testIfNilIfNotNil0ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5 ifNil: block1 ifNotNil: block2) = #bar.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo
]

{ #category : #tests }
OCIfNotNilTests >> testIfNilIfNotNil1Arg [

	self assert: (5 ifNil: [#foo] ifNotNil: [:a | a printString]) = '5'.
	self assert: (nil ifNil: [#foo] ifNotNil: [:a | a printString]) = #foo
]

{ #category : #tests }
OCIfNotNilTests >> testIfNilIfNotNil1ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5 ifNil: block1 ifNotNil: block2) = '5'.
	self assert: (nil ifNil: block1 ifNotNil: block2) = #foo
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNil0Arg [
	self assert: (5 ifNotNil: [ #foo ]) = #foo.
	self assert: (nil ifNotNil: [ #foo ]) isNil
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNil0ArgAsVar [
	| block |
	block := [ #foo ].
	self assert: (5 ifNotNil: block) = #foo.
	self assert: (nil ifNotNil: block) isNil
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNil1Arg [
	self assert: (5 ifNotNil: [ :a | a printString ]) = '5'.
	self assert: (nil ifNotNil: [ :a | a printString ]) isNil
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNil1ArgAsVar [
	| block |
	block := [ :a | a printString ].
	self assert: (5 ifNotNil: block) = '5'.
	self assert: (nil ifNotNil: block) isNil
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNil1ArgWithStatement [
	self assert: (5 ifNotNil: [ :a | 3. a ]) = 5.
	self assert: (5 ifNotNil: [ :a | a. 3 ]) = 3.
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNilIfNil0Arg [

	self assert: (5 ifNotNil: [#foo] ifNil: [#bar]) = #foo.
	self assert: (nil ifNotNil: [#foo] ifNil: [#bar]) = #bar
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNilIfNil0ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [#bar].
	self assert: (5 ifNotNil: block2 ifNil: block1) = #bar.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNilIfNil1Arg [

	self assert: (5 ifNotNil: [:a | a printString] ifNil: [#foo]) = '5'.
	self assert: (nil ifNotNil: [:a | a printString] ifNil: [#foo]) = #foo
]

{ #category : #tests }
OCIfNotNilTests >> testIfNotNilIfNil1ArgAsVar [

	| block1 block2 |
	block1 := [#foo].
	block2 := [:a | a printString].
	self assert: (5 ifNotNil: block2 ifNil: block1) = '5'.
	self assert: (nil ifNotNil: block2 ifNil: block1) = #foo
]
