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
	
	^ aLinkedList
		ifNotEmpty: [|list|list := LinkedList new.
	(self intersectValueLink: aLinkedList firstLink with: aCollection)
		ifNotNil: [ :link | list add: link ].
	 list  ].
	
]

{ #category : #tests }
ContinuationTest >> intersect: aLinkedList with: aCollection continuation: hop [
	^ 
	aCollection ifEmpty: [ hop value: aCollection ]
	ifNotEmpty: [
	aLinkedList
		ifNotEmpty: [ | list |
			list := LinkedList new.
			(self intersectValueLink: aLinkedList firstLink with: aCollection)
				ifNotNil: [ :link | list add: link ].
			list ]]
]

{ #category : #tests }
ContinuationTest >> intersect: aSet withAll: sets [
	^ sets
		ifEmpty: [ aSet ]
		ifNotEmpty: [ self intersect: aSet with: (self intersect: sets first withAll: sets allButFirst ) ]
]

{ #category : #tests }
ContinuationTest >> intersect: aSet withAll: sets continuation: hop [
	^ aSet
		ifEmpty: [ hop value: aSet ]
		ifNotEmpty: [ sets
				ifEmpty: [ aSet ]
				ifNotEmpty: [ self
						intersect: aSet
						with: (self intersect: sets first withAll: sets allButFirst continuation: hop) ] ]
]

{ #category : #tests }
ContinuationTest >> intersectAll: sets [
	^ sets
		ifNotEmpty: [ self intersect: sets first withAll: sets allButFirst ]
]

{ #category : #tests }
ContinuationTest >> intersectAllWithCC: sets [
	^ 
	self callcc: [   :cc | 
	sets
		ifNotEmpty: [ self intersect: sets first withAll: sets allButFirst continuation: cc]]
]

{ #category : #tests }
ContinuationTest >> intersectValueLink: aValueLink with: anotherSet [
	^ aValueLink
		ifNotNil: [ | v nl |
			v := aValueLink value.
			nl := self
				intersectValueLink: aValueLink nextLink
				with: anotherSet.
			(anotherSet includes: v)
				ifTrue: [ ValueLink new
						value: v;
						nextLink: nl;
						yourself ]
				ifFalse: [ nl ] ]
]

{ #category : #tests }
ContinuationTest >> intersectWithCC: aSet withAll: sets continuation: hop [
	^ aSet
		ifEmpty: [ hop value: aSet ]
		ifNotEmpty: [ sets
				ifEmpty: [ aSet ]
				ifNotEmpty: [ self
						intersect: aSet
						with:
							(self
								intersectWithCC: sets first
								withAll: sets allButFirst
								continuation: hop)
						continuation: hop ] ]
]

{ #category : #tests }
ContinuationTest >> intersectWithCCAllWithCC: sets [
	^ self
		callcc: [ :cc | 
			sets
				ifNotEmpty: [ self
						intersectWithCC: sets first
						withAll: sets allButFirst
						continuation: cc ] ]
]

{ #category : #tests }
ContinuationTest >> leftmost: aBlock tree: aTree [
	^ self callcc: [ :skip | self leftmost: aBlock tree: aTree continuation: skip ]
]

{ #category : #tests }
ContinuationTest >> leftmost: aBlock tree: aTree continuation: out [
	^ aTree
		ifNotNil: [ | v |
			v := aTree value.
			v class = ValueLink
				ifTrue: [ self
						leftmost: aBlock tree: v continuation: out;
						leftmost: aBlock tree: aTree nextLink continuation: out ]
				ifFalse: [ (aBlock value: v) ifTrue: [out value: v] ] ]
]

{ #category : #tests }
ContinuationTest >> remove: anObj uptoLast: aLinkedList [
^ aLinkedList ifNotEmpty: [  	|vl| 
	vl := self
		callcc: [ :skip | 
			self
				remove: anObj
				uptoLastValueLink: aLinkedList firstLink
				continuation: skip ].
	LinkedList with: vl]
]

{ #category : #tests }
ContinuationTest >> remove: anObj uptoLastValueLink: aValueLink continuation: skip [
	^ aValueLink
		ifNotNil: [ |v nl|
			v := aValueLink value.
			nl := self
								remove: anObj
								uptoLastValueLink: aValueLink nextLink
								continuation: skip.
			anObj = v
				ifTrue: [ skip
						value:
							(nl) ]
				ifFalse: [ ValueLink new value: v; nextLink: (nl);yourself ] ]
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
ContinuationTest >> testIntersectAll [
	self
		assert:
			(self
				intersectAll:
					{(LinkedList with: 3 with: #mangos with: #and).
					(LinkedList with: 3 with: #kiwis with: #and).
					(LinkedList with: 3 with: #hamburges)})
		equals: {3}.
	self
		assert:
			(self
				intersectAll:
					{(LinkedList with: 3 with: #steaks with: #and).
					(LinkedList with: #no with: #food with: #and).
					(LinkedList with: #three with: #baked with: #potatoes).
					(LinkedList with: 3 with: #diet with: #hamburges)})
		equals: {}.
	self
		assert:
			(self
				intersectAll:
					{(LinkedList with: 3 with: #steaks with: #and).
					LinkedList new.
					(LinkedList with: #three with: #baked with: #potatoes).
					(LinkedList with: 3 with: #diet with: #hamburges)})
		equals: {}.
	self
		assert:
			(self
				intersectAllWithCC:
					{(LinkedList with: 3 with: #steaks with: #and).
					LinkedList new.
					(LinkedList with: #three with: #baked with: #potatoes).
					(LinkedList with: 3 with: #diet with: #hamburges)})
		equals: {}.
	self
		assert:
			(self
				intersectWithCCAllWithCC:
					{(LinkedList with: 3 with: #steaks with: #and).
					(LinkedList with: #no with: #food with: #and).
					(LinkedList with: #three with: #baked with: #potatoes).
					(LinkedList with: 3 with: #diet with: #hamburges)})
		equals: {}
]

{ #category : #tests }
ContinuationTest >> testIntersectWith [
	self
		assert:
			(self
				intersect: (LinkedList with: #apple with: #banana with: #avocado)
				with: (LinkedList with: 1 with: 2 with: #apple))
		equals: (LinkedList with: #apple).
	self
		assert: (self intersect: ((1 to: 10) as: LinkedList) with: (9 to: 20))
		equals: (9 to: 10).
	self
		assert: (self intersect: ((1 to: 10) as: LinkedList) with: (11 to: 20))
		equals: {  }
]

{ #category : #tests }
ContinuationTest >> testLeftmost [
	| tree |
	"`tree` corresponds to the sexp '(((a)) 4 (1))"
	tree := ValueLink new
		value:
			(ValueLink new
				value:
					(ValueLink new
						value: #a;
						yourself);
				yourself);
		nextLink:
			(ValueLink new
				value: 4;
				nextLink:
					(ValueLink new
						value:
							(ValueLink new
								value: 1;
								yourself);
						yourself);
				yourself);
		yourself.
	self assert: (self leftmost: #isSymbol tree: tree) equals: #a.
	self assert: (self leftmost: #isInteger tree: tree) equals: 4.
	self assert: (self leftmost: #isNil tree: tree) equals: nil.
	self assert: (self leftmost: [ :v | v = 5 ] tree: tree) equals: nil
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
ContinuationTest >> testRemoveUptoLast [
	self
		assert:
			(self
				remove: #cookies
				uptoLast:
					(LinkedList new
						add: #cookies;
						add: #chocolate;
						add: #mints;
						add: #caramel;
						add: #delight;
						add: #ginger;
						add: #snaps;
						add: #desserts;
						add: #chocolate;
						add: #mousse;
						add: #vanilla;
						add: #ice;
						add: #cream;
						add: #German;
						add: #chocolate;
						add: #cake;
						add: #more;
						add: #cookies;
						"from this down to the last should be kept in the result
						because the previous #cookies is the last one #cookies in the list."
							add: #gingerbreadman;
						add: #chocolate;
						add: #chip;
						add: #brownies;
						yourself))
		equals:
			(LinkedList new
				add: #gingerbreadman;
				add: #chocolate;
				add: #chip;
				add: #brownies;
				yourself)
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
