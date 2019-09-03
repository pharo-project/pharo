"
I represent intentend to provide complete coverage for the Continuation class.
"
Class {
	#name : #ContinuationTest,
	#superclass : #TestCase,
	#instVars : [
		'tmp',
		'tmp2'
	],
	#category : #'Kernel-Tests-Methods'
}

{ #category : #utilities }
ContinuationTest >> callcc: aBlock [
	^ Continuation currentDo: aBlock
]

{ #category : #tests }
ContinuationTest >> intersect: aLinkedList with: aCollection [
	|vl|
	vl := aLinkedList ifEmpty: [ nil ] ifNotEmpty: [ aLinkedList firstLink ].
	^ LinkedList new add: (self intersectValueLink: vl with: aCollection); yourself
]

{ #category : #tests }
ContinuationTest >> intersectValueLink: aValueLink with: anotherSet [
	^ aValueLink
		ifNotNil: [ | v nl rest |
			v := aValueLink value.
			nl := aValueLink nextLink.
			rest := self intersectValueLink: nl with: anotherSet.
			(anotherSet includes: v)
				ifTrue: [ |  |
					
					ValueLink new
						value: v;
						nextLink: rest;
						yourself ]
				ifFalse: [ rest ] ]
]

{ #category : #tests }
ContinuationTest >> testBlockEscape [
	| x |
	tmp := 0.
	x := [ tmp := tmp + 1. tmp2 value ].
	self callcc: [ :cc | tmp2 := cc. x value ].
	tmp2 := [ ].
	x value.
	self assert: tmp = 2
]

{ #category : #tests }
ContinuationTest >> testBlockTemps [
	| y |
	#(1 2 3) do: [ :i |
		| x |
		x := i.
		tmp ifNil: [ tmp2 := (self callcc: [ :cc | tmp := cc. [ :q | ] ]) ].
		tmp2 value: x.
		x := 17 ].
	y := (self callcc: [ :cc | tmp value: cc. 42 ]).
	self assert: y = 1
]

{ #category : #tests }
ContinuationTest >> testBlockVars [
	| continuation |
	tmp := 0.
	tmp := (self callcc: [ :cc | continuation := cc. 0 ]) + tmp.
	tmp2
		ifNotNil: [ tmp2 value ]
		ifNil: [
			#(1 2 3) do: [ :i |
				self callcc: [ :cc | tmp2 := cc. continuation value: i ] ] ].
	self assert: tmp = 6
]

{ #category : #tests }
ContinuationTest >> testIntersectWith [
	self assert: (self
		intersect: (LinkedList with: #apple with: #banana with: #avocado)
		with: (LinkedList with: 1 with: 2 with: #apple))
		equals: (LinkedList with: #apple)
]

{ #category : #tests }
ContinuationTest >> testMethodTemps [
	| i continuation |
	i := 0.
	i := i + (self callcc: [:cc | continuation := cc. 1]).
	self assert: i ~= 3.
	i = 2 ifFalse: [ continuation value: 2 ]
]

{ #category : #tests }
ContinuationTest >> testReentrant [
	| assoc |
	assoc := self callcc: [ :cc | cc -> 0 ].
	assoc value: assoc value + 1.
	self assert: assoc value ~= 5.
	assoc value = 4
		ifFalse: [ assoc key value: assoc ]
]

{ #category : #tests }
ContinuationTest >> testSimpleCallCC [
	| x continuation v |
	v := 0.
	x := self
		callcc: [ :cc | 
			continuation := cc.
			false ].
	x
		ifTrue: [ v := v + 1 ]
		ifFalse: [ continuation value: true.
			self error: 'This message shouldn''t be sent, ever.' ].
	self assert: x.
	self assert: v equals: 1
]

{ #category : #tests }
ContinuationTest >> testSimpleStoreCallCC [
	| x continuation |
	
	continuation := self callcc: #yourself.
	x := false.continuation = true
		ifTrue: [ x := true ]
		ifFalse: [ continuation value: true.
			self error: 'This message shouldn''t be sent, ever.' ].
	self assert: x
]

{ #category : #tests }
ContinuationTest >> testSimplestCallCC [
	| x |
	x := self callcc: [ :cc | (cc value: true) not ].
	self assert: x
]
