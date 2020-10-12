"
Basic Collection tests
"
Class {
	#name : #CollectionTest,
	#superclass : #TestCase,
	#category : #'Collections-Support-Tests-Base'
}

{ #category : #test }
CollectionTest >> testCopyWithoutDuplicates [
 	| a b |
	a := #(1 2 3 4 4 5 6 1) copyWithoutDuplicates.
	b := #(4 2 3 1 6 5).
	self assert: [(a includesAll: b) and: [ b includesAll: a ]].

	a := #('do' 'la' 'si' 'do' 'la') copyWithoutDuplicates .
	b := #('do' 'la' 'si' ).
	self assert: [(a includesAll: b) and: [ b includesAll: a ]].

	a := #(#do #la #si #do #la) copyWithoutDuplicates.
	b := #(#si #la #do).
	self assert: [(a includesAll: b) and: [ b includesAll: a ]].
]

{ #category : #tests }
CollectionTest >> testGroupedByGroupsOrderSimple [
	| dataCollection grouped keys |
	"test of preserving groups order according to source collection order"
	dataCollection := OrderedCollection new
		add: #('ZZZ' '1');
		add: #('ZZZ' '2');
		add: #('ZZZ' '3');
		add: #('SSS' '4');
		add: #('SSS' '5');
		add: #('SSS' '6');
		add: #('BFLM' 'X');
		add: #('AAA' '1');
		add: #('AAA' '2');
		yourself.
	
	grouped := dataCollection groupedBy: [ :arrItem | arrItem at: 1 ].
	keys := grouped keys.
	self assert: (keys at: 1) equals: 'ZZZ'.
	self assert: (keys at: 2) equals: 'SSS'.
	self assert: (keys at: 3) equals: 'BFLM'.
	self assert: (keys at: 4) equals: 'AAA'.
	
	"---"
	
	dataCollection := OrderedCollection new
		add: #('k' 1);
		add: #('f' 2);
		add: #('k' 3);
		add: #('k' 4);
		add: #('k' 5);
		add: #('f' 6);
		add: #('f' 7);
		add: #('k' 8);
		yourself.
	grouped := dataCollection groupedBy: [ :arrItem | arrItem at: 1 ].
	keys := grouped keys.
	self assert: (keys at: 1) equals: 'k'.
	self assert: (keys at: 2) equals: 'f'.
	self assert: (grouped at: 'k') asArray equals: #(#('k' 1) #('k' 3) #('k' 4) #('k' 5) #('k' 8)).
	self assert: (grouped at: 'f') asArray equals: #(#('f' 2) #('f' 6) #('f' 7)).
]

{ #category : #tests }
CollectionTest >> testGroupedByGroupsOrderWithSortedCollectionOfDates [
	| dataCollection grouped keys |
	"data collection full of Date instances is sorted ascending"
	dataCollection := OrderedCollection new.
	0 to: 10 do: [ :i | 0 to: 5 do: [ :j | dataCollection add: (Date year: 2000 month: 1 day: 1) + i years + j weeks ] ].
	
	"group dates collection by year"
	grouped := dataCollection groupedBy: [ :dateItem | dateItem year ].

	"test if groups (Years) order is preserved after groupedBy"	
	keys := grouped keys.
	1 to: grouped size do: [ :i |
		self assert: (keys at: i) equals: 2000 + i - 1.
		].

]

{ #category : #test }
CollectionTest >> testTopologicalSortOnCycleDo [

	"This test stress four small cycles detection."

	self
		assert: ({ (1 -> 1) } topologicalSortOnCycleDo: [ :cycle | cycle ])
		equals: { (1 -> 1) }.
	self
		assert: ({ 
				 (1 -> 2).
				 (2 -> 1) } topologicalSortOnCycleDo: [ :cycle | cycle ])
		equals: { 
				(2 -> 1).
				(1 -> 2) }.
	self
		assert: ({ 
				 (1 -> 2).
				 (2 -> 3).
				 (3 -> 1) } topologicalSortOnCycleDo: [ :cycle | cycle ])
		equals: { 
				(3 -> 1).
				(2 -> 3).
				(1 -> 2) }.
	self
		assert: ({ 
				 (5 -> 1).
				 (1 -> 2).
				 (2 -> 3).
				 (3 -> 5).
				 (3 -> 4) } topologicalSortOnCycleDo: [ :cycle | cycle ])
		equals: { 
				(5 -> 1).
				(3 -> 5).
				(2 -> 3).
				(1 -> 2) }
]

{ #category : #tests }
CollectionTest >> testTopologicalSortOnCycleDo1 [

	"The following test is kept from TAOCP by Donald Knuth, Volume 1 page 272."

	self
		assert: ({ 
				 (9 -> 2).
				 (3 -> 7).
				 (7 -> 5).
				 (5 -> 8).
				 (8 -> 6).
				 (4 -> 6).
				 (1 -> 3).
				 (7 -> 4).
				 (9 -> 5).
				 (2 -> 8) } topologicalSortOnCycleDo: [ Error signal ])
		equals: #( 1 9 3 2 7 4 5 8 6 )
]

{ #category : #tests }
CollectionTest >> testTopologicalSortOnCycleDo2 [

	"The following test is kept from https://upload.wikimedia.org/wikipedia/commons/0/03/Directed_acyclic_graph_2.svg."

	self
		assert: ({ 
				 (1 -> 4).
				 (2 -> 4).
				 (2 -> 5).
				 (3 -> 5).
				 (3 -> 8).
				 (4 -> 6).
				 (4 -> 7).
				 (4 -> 8).
				 (5 -> 7) } topologicalSortOnCycleDo: [ Error signal ])
		equals: #( 1 2 3 4 5 8 6 7 )
]

{ #category : #tests }
CollectionTest >> testTopologicalSortOnCycleDo3 [

	| random assocs |
	random := Random seed: 11.
	assocs := (1 to: 10000) collect: [ :k | 
		          | a b |
		          a := random nextInt: 100.
		          b := random nextInt: 100.
		          a -> (a = b
			           ifTrue: [ b + 1 \\ 100 max: 1 ]
			           ifFalse: [ b ]) ].
	self
		assert: (assocs topologicalSortOnCycleDo: [ :cycle | cycle ])
		equals: { 
				(94 -> 100).
				(100 -> 94) }
]

{ #category : #test }
CollectionTest >> testTopologicalSortOnCycleDo5 [

	"This test stress four small cycles detection."

	self
		should: [ 
			{ 
				(1 -> 2).
				(4 -> 1) } topologicalSortOnCycleDo: [ :cycle | Error signal ] ]
		raise: SubscriptOutOfBounds
]

{ #category : #tests }
CollectionTest >> testTopologicalSortOnCycleDo6 [

	"This test case shows how to iteratively removing edges belonging to cycle
 	 yield an acyclic relation."

	| relations random edges |
	random := Random seed: 7.
	relations := (1 to: 1000)
		             collect: [ :k | 
			             | a b |
			             a := random nextInt: 100.
			             b := random nextInt: 100.
			             a -> (a = b
				              ifTrue: [ b + 1 \\ 100 max: 1 ]
				              ifFalse: [ b ]) ]
		             as: OrderedCollection.

	edges := Array streamContents: [ :array | 
		         [ 
		         | isAcyclic |
		         isAcyclic := true.
		         relations := relations topologicalSortOnCycleDo: [ :cycle | 
			                      | edge |
			                      isAcyclic := false.
			                      edge := cycle anyOne.
			                      array nextPut: edge.
			                      relations
				                      remove: edge;
				                      yourself ].
		         isAcyclic ] whileFalse ].

	self
		assert: relations
		equals:
			#( 60 73 86 94 95 99 100 57 96 90 87 98 82 80 64 1 88 89 84 92 93
			   15 2 69 97 41 63 67 81 76 7 75 44 68 37 3 65 47 70 66 85 91 71
			   59 4 62 77 50 58 83 72 46 9 5 56 42 48 11 49 38 6 39 45 8 16 27
			   78 74 30 79 34 40 20 19 36 14 24 53 35 33 12 25 55 51 26 13 23
			   21 52 28 17 31 22 18 32 43 29 61 10 54 ).

	self assert: edges equals: { 
			(99 -> 100).
			(94 -> 100).
			(97 -> 96).
			(91 -> 96).
			(88 -> 96).
			(83 -> 96).
			(59 -> 96).
			(40 -> 96).
			(23 -> 96).
			(22 -> 96).
			(90 -> 95).
			(80 -> 95).
			(78 -> 95).
			(95 -> 87).
			(89 -> 87).
			(75 -> 87).
			(71 -> 87).
			(69 -> 87).
			(55 -> 87).
			(50 -> 87).
			(50 -> 87).
			(49 -> 87).
			(38 -> 87).
			(3 -> 87).
			(61 -> 95).
			(51 -> 95).
			(41 -> 95).
			(95 -> 98).
			(98 -> 99).
			(85 -> 99).
			(81 -> 99).
			(72 -> 99).
			(71 -> 99).
			(99 -> 93).
			(92 -> 93).
			(71 -> 93).
			(70 -> 93).
			(67 -> 93).
			(64 -> 93).
			(53 -> 93).
			(50 -> 93).
			(31 -> 93).
			(24 -> 93).
			(16 -> 93).
			(93 -> 80).
			(65 -> 80).
			(21 -> 80).
			(11 -> 80).
			(89 -> 73).
			(68 -> 73).
			(62 -> 73).
			(57 -> 73).
			(46 -> 73).
			(36 -> 73).
			(30 -> 73).
			(25 -> 73).
			(21 -> 73).
			(13 -> 73).
			(11 -> 73).
			(7 -> 73).
			(69 -> 64).
			(64 -> 99).
			(95 -> 82).
			(90 -> 82).
			(89 -> 82).
			(81 -> 82).
			(75 -> 82).
			(72 -> 82).
			(68 -> 82).
			(64 -> 82).
			(64 -> 82).
			(82 -> 60).
			(61 -> 60).
			(56 -> 60).
			(52 -> 60).
			(46 -> 60).
			(41 -> 60).
			(16 -> 60).
			(3 -> 60).
			(47 -> 99).
			(85 -> 94).
			(75 -> 94).
			(74 -> 94).
			(65 -> 94).
			(62 -> 94).
			(43 -> 94).
			(46 -> 99).
			(42 -> 99).
			(81 -> 98).
			(77 -> 98).
			(71 -> 98).
			(58 -> 98).
			(88 -> 57).
			(78 -> 57).
			(76 -> 57).
			(64 -> 57).
			(22 -> 57).
			(21 -> 57).
			(21 -> 57).
			(9 -> 57).
			(5 -> 57).
			(4 -> 57).
			(36 -> 98).
			(27 -> 98).
			(13 -> 95).
			(10 -> 95).
			(95 -> 90).
			(89 -> 90).
			(89 -> 90).
			(86 -> 90).
			(85 -> 90).
			(70 -> 90).
			(59 -> 90).
			(38 -> 90).
			(7 -> 90).
			(95 -> 85).
			(58 -> 82).
			(55 -> 82).
			(36 -> 82).
			(16 -> 82).
			(12 -> 82).
			(78 -> 97).
			(70 -> 97).
			(62 -> 97).
			(51 -> 97).
			(42 -> 97).
			(28 -> 97).
			(24 -> 97).
			(19 -> 97).
			(19 -> 97).
			(18 -> 97).
			(64 -> 88).
			(62 -> 88).
			(56 -> 88).
			(15 -> 88).
			(88 -> 14).
			(83 -> 14).
			(74 -> 14).
			(65 -> 14).
			(14 -> 93).
			(9 -> 93).
			(47 -> 1).
			(30 -> 1).
			(9 -> 1).
			(8 -> 1).
			(8 -> 1).
			(83 -> 92).
			(72 -> 92).
			(71 -> 92).
			(68 -> 92).
			(92 -> 86).
			(81 -> 86).
			(74 -> 86).
			(63 -> 86).
			(50 -> 86).
			(44 -> 86).
			(25 -> 86).
			(15 -> 86).
			(14 -> 86).
			(49 -> 92).
			(38 -> 92).
			(13 -> 92).
			(14 -> 88).
			(67 -> 64).
			(63 -> 64).
			(63 -> 64).
			(79 -> 85).
			(69 -> 85).
			(58 -> 64).
			(58 -> 64).
			(53 -> 64).
			(44 -> 64).
			(34 -> 64).
			(21 -> 64).
			(10 -> 64).
			(7 -> 64).
			(70 -> 41).
			(55 -> 41).
			(35 -> 41).
			(35 -> 41).
			(29 -> 41).
			(24 -> 41).
			(21 -> 41).
			(41 -> 15).
			(28 -> 15).
			(16 -> 15).
			(6 -> 15).
			(12 -> 92).
			(61 -> 85).
			(42 -> 85).
			(29 -> 85).
			(28 -> 85).
			(68 -> 81).
			(62 -> 81).
			(47 -> 81).
			(44 -> 81).
			(24 -> 81).
			(85 -> 66).
			(65 -> 66).
			(83 -> 63).
			(70 -> 63).
			(65 -> 63).
			(58 -> 63).
			(52 -> 63).
			(50 -> 63).
			(44 -> 63).
			(41 -> 63).
			(36 -> 63).
			(10 -> 63).
			(63 -> 2).
			(51 -> 2).
			(49 -> 2).
			(39 -> 2).
			(28 -> 2).
			(23 -> 2).
			(49 -> 66).
			(4 -> 95).
			(84 -> 89).
			(35 -> 89).
			(26 -> 89).
			(26 -> 89).
			(17 -> 89).
			(13 -> 89).
			(10 -> 89).
			(74 -> 84).
			(66 -> 84).
			(48 -> 84).
			(47 -> 84).
			(40 -> 84).
			(24 -> 84).
			(4 -> 84).
			(91 -> 76).
			(75 -> 76).
			(72 -> 76).
			(69 -> 76).
			(69 -> 76).
			(62 -> 76).
			(54 -> 76).
			(50 -> 76).
			(35 -> 76).
			(33 -> 76).
			(25 -> 76).
			(6 -> 76).
			(70 -> 91).
			(91 -> 69).
			(85 -> 69).
			(51 -> 69).
			(34 -> 69).
			(33 -> 69).
			(31 -> 69).
			(23 -> 69).
			(14 -> 69).
			(47 -> 91).
			(65 -> 44).
			(45 -> 44).
			(11 -> 44).
			(4 -> 44).
			(41 -> 91).
			(38 -> 91).
			(85 -> 68).
			(78 -> 68).
			(43 -> 68).
			(39 -> 68).
			(28 -> 68).
			(21 -> 68).
			(18 -> 85).
			(10 -> 85).
			(9 -> 85).
			(85 -> 67).
			(78 -> 67).
			(75 -> 67).
			(30 -> 67).
			(28 -> 67).
			(25 -> 67).
			(5 -> 67).
			(40 -> 3).
			(38 -> 3).
			(31 -> 3).
			(20 -> 3).
			(20 -> 3).
			(18 -> 3).
			(17 -> 3).
			(11 -> 3).
			(62 -> 70).
			(46 -> 37).
			(36 -> 37).
			(33 -> 37).
			(22 -> 37).
			(8 -> 37).
			(20 -> 70).
			(65 -> 71).
			(50 -> 71).
			(38 -> 71).
			(34 -> 66).
			(56 -> 62).
			(30 -> 66).
			(29 -> 66).
			(25 -> 66).
			(59 -> 78).
			(47 -> 78).
			(38 -> 78).
			(78 -> 77).
			(46 -> 77).
			(41 -> 77).
			(30 -> 77).
			(17 -> 77).
			(16 -> 77).
			(6 -> 77).
			(75 -> 72).
			(34 -> 72).
			(25 -> 72).
			(75 -> 65).
			(59 -> 65).
			(40 -> 62).
			(34 -> 62).
			(26 -> 62).
			(12 -> 62).
			(8 -> 62).
			(5 -> 62).
			(51 -> 55).
			(55 -> 50).
			(46 -> 50).
			(16 -> 50).
			(61 -> 4).
			(50 -> 4).
			(50 -> 4).
			(48 -> 4).
			(46 -> 4).
			(29 -> 4).
			(21 -> 4).
			(19 -> 4).
			(11 -> 4).
			(55 -> 39).
			(33 -> 78).
			(78 -> 75).
			(45 -> 75).
			(28 -> 75).
			(21 -> 91).
			(49 -> 83).
			(41 -> 83).
			(21 -> 83).
			(11 -> 83).
			(48 -> 65).
			(28 -> 65).
			(12 -> 65).
			(61 -> 58).
			(11 -> 58).
			(34 -> 54).
			(13 -> 41).
			(8 -> 41).
			(27 -> 59).
			(23 -> 59).
			(13 -> 48).
			(43 -> 5).
			(28 -> 5).
			(28 -> 5).
			(24 -> 5).
			(6 -> 5).
			(25 -> 46).
			(33 -> 54).
			(47 -> 56).
			(45 -> 56).
			(39 -> 56).
			(17 -> 49).
			(12 -> 49).
			(26 -> 54).
			(54 -> 45).
			(47 -> 45).
			(31 -> 45).
			(20 -> 45).
			(16 -> 6).
			(13 -> 6).
			(36 -> 8).
			(30 -> 8).
			(22 -> 8).
			(18 -> 8).
			(16 -> 8).
			(13 -> 8).
			(38 -> 74).
			(13 -> 9).
			(51 -> 32).
			(61 -> 42).
			(40 -> 42).
			(40 -> 42).
			(33 -> 42).
			(29 -> 42).
			(52 -> 39).
			(21 -> 38).
			(36 -> 39).
			(54 -> 47).
			(22 -> 27).
			(35 -> 32).
			(43 -> 30).
			(32 -> 53).
			(28 -> 53).
			(53 -> 20).
			(43 -> 20).
			(17 -> 20).
			(21 -> 24).
			(18 -> 19).
			(12 -> 24).
			(10 -> 24).
			(13 -> 35).
			(54 -> 13).
			(52 -> 13).
			(21 -> 12).
			(21 -> 12).
			(18 -> 12).
			(10 -> 12).
			(31 -> 28).
			(22 -> 28).
			(29 -> 22).
			(18 -> 22).
			(18 -> 22).
			(10 -> 22) }
]
