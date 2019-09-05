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

{ #category : #functions }
ContinuationTest >> intersect: aLinkedList with: aCollection [
	
	^ aLinkedList
		ifNotEmpty: [|list|list := LinkedList new.
	(self intersectValueLink: aLinkedList firstLink with: aCollection)
		ifNotNil: [ :link | list add: link ].
	 list  ].
	
]

{ #category : #private }
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

{ #category : #private }
ContinuationTest >> intersect: aSet withAll: sets [
	^ sets
		ifEmpty: [ aSet ]
		ifNotEmpty: [ self intersect: aSet with: (self intersect: sets first withAll: sets allButFirst ) ]
]

{ #category : #private }
ContinuationTest >> intersect: aSet withAll: sets continuation: hop [
	^ aSet
		ifEmpty: [ hop value: aSet ]
		ifNotEmpty: [ sets
				ifEmpty: [ aSet ]
				ifNotEmpty: [ self
						intersect: aSet
						with: (self intersect: sets first withAll: sets allButFirst continuation: hop) ] ]
]

{ #category : #functions }
ContinuationTest >> intersectAll: sets [
	^ sets
		ifNotEmpty: [ self intersect: sets first withAll: sets allButFirst ]
]

{ #category : #functions }
ContinuationTest >> intersectAllWithCC: sets [
	^ 
	self callcc: [   :cc | 
	sets
		ifNotEmpty: [ self intersect: sets first withAll: sets allButFirst continuation: cc]]
]

{ #category : #private }
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

{ #category : #private }
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

{ #category : #functions }
ContinuationTest >> intersectWithCCAllWithCC: sets [
	^ self
		callcc: [ :cc | 
			sets
				ifNotEmpty: [ self
						intersectWithCC: sets first
						withAll: sets allButFirst
						continuation: cc ] ]
]

{ #category : #functions }
ContinuationTest >> leftmost: aBlock tree: aTree [
	^ self callcc: [ :skip | self leftmost: aBlock tree: aTree continuation: skip ]
]

{ #category : #private }
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

{ #category : #utilities }
ContinuationTest >> printStringOfTree: aValueLink [
	^ String
		streamContents: [ :aStream | self printStringOfTree: aValueLink onStream: aStream]
]

{ #category : #utilities }
ContinuationTest >> printStringOfTree: aValueLink onStream: aStream [
	| cell |
	aStream nextPut: $(.
	cell := aValueLink.
	[ | car |
	car := cell value.
	car class = ValueLink
		ifTrue: [ self printStringOfTree: car onStream: aStream ]
		ifFalse: [ car printOn: aStream ].
	cell nextLink ifNotNil: [ aStream nextPut: Character space ].
	cell := cell nextLink.
	cell class = ValueLink ] whileTrue.
	cell
		ifNotNil: [ aStream
				nextPut: $.;
				nextPut: Character space.
			cell printOn: aStream ].
	aStream nextPut: $)
]

{ #category : #functions }
ContinuationTest >> remove: anObj oneStar: aTree [
	| newTree sentinel |
	sentinel := #absent.
	newTree := self
		callcc: [ :oh | 
			self
				remove: anObj
				oneStar: aTree
				sentinel: sentinel
				continuation: oh ].
	^ newTree = sentinel
		ifTrue: [ aTree ]
		ifFalse: [ newTree ]
]

{ #category : #private }
ContinuationTest >> remove: anObj oneStar: aTree sentinel: aSymbol continuation: oh [
	^ aTree
		ifNil: [ oh value: aSymbol ]
		ifNotNil: [ | v nl naturalRecursion |
			v := aTree value.
			nl := aTree nextLink.
			naturalRecursion := [ ValueLink new
				value: v;
				nextLink:
					(self
						remove: anObj
						oneStar: nl
						sentinel: aSymbol
						continuation: oh);
				yourself ].
			v class = ValueLink
				ifTrue: [ | car |
					car := self
						callcc: [ :ooh | 
							self
								remove: anObj
								oneStar: v
								sentinel: aSymbol
								continuation: ooh ].
					car = aSymbol
						ifTrue:  naturalRecursion 
						ifFalse: [ ValueLink new
								value: car;
								nextLink: nl;
								yourself ] ]
				ifFalse: [ anObj = v
						ifTrue: [ nl ]
						ifFalse:  naturalRecursion  ] ]
]

{ #category : #functions }
ContinuationTest >> remove: anObj oneStarWithTry: aTree [
	
	^ Continuation try: [ :oh | self remove: anObj oneStarWithTry: aTree  continuation: oh ] otherwise: [ aTree ]
]

{ #category : #private }
ContinuationTest >> remove: anObj oneStarWithTry: aTree continuation: oh [
	^ aTree
		ifNil: [ oh value: #absent ]
		ifNotNil: [ | v nl naturalRecursion |
			v := aTree value.
			nl := aTree nextLink.
			naturalRecursion := [ ValueLink new
				value: v;
				nextLink: (self remove: anObj oneStarWithTry: nl continuation: oh);
				yourself ].
			v class = ValueLink
				ifTrue: [ Continuation
						try: [ :ooh | 
							ValueLink new
								value: (self remove: anObj oneStarWithTry: v continuation: ooh);
								nextLink: nl;
								yourself ]
						otherwise:  naturalRecursion  ]
				ifFalse: [ anObj = v
						ifTrue: [ nl ]
						ifFalse: naturalRecursion ] ]
]

{ #category : #functions }
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

{ #category : #private }
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
ContinuationTest >> testRemoveOneStar [
	| tree1 tree2 |
	tree1 := ValueLink new
		value:
			(ValueLink new
				value: #Swedish;
				nextLink:
					(ValueLink new
						value: #rye;
						yourself);
				yourself);
		nextLink:
			(ValueLink new
				value:
					(ValueLink new
						value: #French;
						nextLink:
							(ValueLink new
								value:
									(ValueLink new
										value: #mustard;
										nextLink:
											(ValueLink new
												value: #salad;
												nextLink:
													(ValueLink new
														value: #turkey;
														yourself);
												yourself);
										yourself);
								yourself);
						yourself);
				nextLink:
					(ValueLink new
						value: #salad;
						yourself);
				yourself);
		yourself.
	tree2 := ValueLink new
		value:
			(ValueLink new
				value: #pasta;
				nextLink:
					(ValueLink new
						value: #meat;
						yourself);
				yourself);
		nextLink:
			(ValueLink new
				value: #pasta;
				nextLink:
					(ValueLink new
						value:
							(ValueLink new
								value: #noodles;
								nextLink:
									(ValueLink new
										value: #meat;
										nextLink:
											(ValueLink new
												value: #sauce;
												yourself);
										yourself);
								yourself);
						nextLink:
							(ValueLink new
								value: #meat;
								nextLink:
									(ValueLink new
										value: #tomatoes;
										yourself);
								yourself);
						yourself);
				yourself);
		yourself.
	self
		assert: (self printStringOfTree: tree1)
		equals: '((#Swedish #rye) (#French (#mustard #salad #turkey)) #salad)'.
	self
		assert: (self printStringOfTree: tree2)
		equals: '((#pasta #meat) #pasta (#noodles #meat #sauce) #meat #tomatoes)'.
	self
		assert: (self printStringOfTree: (self remove: #salad oneStar: tree1))
		equals: '((#Swedish #rye) (#French (#mustard #turkey)) #salad)'.
	self
		assert: (self printStringOfTree: (self remove: #salad oneStarWithTry: tree1))
		equals: '((#Swedish #rye) (#French (#mustard #turkey)) #salad)'.
	self
		assert: (self printStringOfTree: (self remove: #meat oneStar: tree2))
		equals: '((#pasta) #pasta (#noodles #meat #sauce) #meat #tomatoes)'.
	self
		assert: (self printStringOfTree: (self remove: #meat oneStarWithTry: tree2))
		equals: '((#pasta) #pasta (#noodles #meat #sauce) #meat #tomatoes)'
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
