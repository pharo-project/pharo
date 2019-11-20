Class {
	#name : #OCIfNotNilTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-FromOld'
}

{ #category : #tests }
OCIfNotNilTest >> testIfNilIfNotNil0Arg [
	self assert: (5 ifNil: [ #foo ] ifNotNil: [ #bar ]) equals: #bar.
	self assert: (nil ifNil: [ #foo ] ifNotNil: [ #bar ]) equals: #foo
]

{ #category : #tests }
OCIfNotNilTest >> testIfNilIfNotNil0ArgAsVar [
	| block1 block2 |
	block1 := [ #foo ].
	block2 := [ #bar ].
	self assert: (5 ifNil: block1 ifNotNil: block2) equals: #bar.
	self assert: (nil ifNil: block1 ifNotNil: block2) equals: #foo
]

{ #category : #tests }
OCIfNotNilTest >> testIfNilIfNotNil1Arg [
	self assert: (5 ifNil: [ #foo ] ifNotNil: [ :a | a printString ]) equals: '5'.
	self assert: (nil ifNil: [ #foo ] ifNotNil: [ :a | a printString ]) equals: #foo
]

{ #category : #tests }
OCIfNotNilTest >> testIfNilIfNotNil1ArgAsVar [
	| block1 block2 |
	block1 := [ #foo ].
	block2 := [ :a | a printString ].
	self assert: (5 ifNil: block1 ifNotNil: block2) equals: '5'.
	self assert: (nil ifNil: block1 ifNotNil: block2) equals: #foo
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNil0Arg [
	self assert: (5 ifNotNil: [ #foo ]) equals: #foo.
	self assert: (nil ifNotNil: [ #foo ]) isNil
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNil0ArgAsVar [
	| block |
	block := [ #foo ].
	self assert: (5 ifNotNil: block) equals: #foo.
	self assert: (nil ifNotNil: block) isNil
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNil1Arg [
	self assert: (5 ifNotNil: [ :a | a printString ]) equals: '5'.
	self assert: (nil ifNotNil: [ :a | a printString ]) isNil
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNil1ArgAsVar [
	| block |
	block := [ :a | a printString ].
	self assert: (5 ifNotNil: block) equals: '5'.
	self assert: (nil ifNotNil: block) isNil
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNil1ArgWithStatement [
	self
		assert:
			(5
				ifNotNil: [ :a | 
					3.
					a ])
		equals: 5.
	self
		assert:
			(5
				ifNotNil: [ :a | 
					a.
					3 ])
		equals: 3
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNilIfNil0Arg [
	self assert: (5 ifNotNil: [ #foo ] ifNil: [ #bar ]) equals: #foo.
	self assert: (nil ifNotNil: [ #foo ] ifNil: [ #bar ]) equals: #bar
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNilIfNil0ArgAsVar [
	| block1 block2 |
	block1 := [ #foo ].
	block2 := [ #bar ].
	self assert: (5 ifNotNil: block2 ifNil: block1) equals: #bar.
	self assert: (nil ifNotNil: block2 ifNil: block1) equals: #foo
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNilIfNil1Arg [
	self assert: (5 ifNotNil: [ :a | a printString ] ifNil: [ #foo ]) equals: '5'.
	self assert: (nil ifNotNil: [ :a | a printString ] ifNil: [ #foo ]) equals: #foo
]

{ #category : #tests }
OCIfNotNilTest >> testIfNotNilIfNil1ArgAsVar [
	| block1 block2 |
	block1 := [ #foo ].
	block2 := [ :a | a printString ].
	self assert: (5 ifNotNil: block2 ifNil: block1) equals: '5'.
	self assert: (nil ifNotNil: block2 ifNil: block1) equals: #foo
]
