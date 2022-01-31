"
I'm a test class for testing the behavior of ordered list blocks
"
Class {
	#name : #MicOrderedListBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #tests }
MicOrderedListBlockTest >> testEmbeddedUnordered [
	| source root list |
	source := 
'1. item 1
   - sublevel item 1
   - sublevel item 2
1. item 2'.
	root := parser parse: source.
	list := root children first.
	self assert: list children size equals: 2.
	self assert: list children second text equals: 'item 2'.
	self assert: list children first "item 1"
			children first "unordered list"
			children second "sub item"
			text equals: 'sublevel item 2'
]

{ #category : #'tests - nested' }
MicOrderedListBlockTest >> testNestedLevel [
	| source root list |
	source := 
'1. item 1
   6) sublevel
1. item 2'.
	root := parser parse: source.
	list := root children first.
	self assert: list children size equals: 2.
	
	self assert: list children first text equals: 'item 1'.
	self assert: list children first nestedLevel equals: 1.
	
	"this is the inner list"
	self assert: list children first children first nestedLevel equals: 2.
	
	"this is the inner list first item"
	self assert: list children first children first children first text equals: 'sublevel'.
	self assert: list children first children first children first nestedLevel equals: 2.
	

]

{ #category : #'tests - nested' }
MicOrderedListBlockTest >> testNestedLevel2 [
	| source root list |
	source := 
'1. item 1
   6) sublevel
      1. subsublevel
   7) sublevel2
1. item 2'.
	root := parser parse: source.
	list := root children first.
	self assert: list children size equals: 2.
	
	self assert: list children first text equals: 'item 1'.
	self assert: list children first nestedLevel equals: 1.
	
	"this is the inner list"
	self assert: list children first children first nestedLevel equals: 2.
	
	"this is the inner list first item"
	self assert: list children first children first children first text equals: 'sublevel'.
	self assert: list children first children first children first nestedLevel equals: 2.
	self assert: list children first children first children second text equals: 'sublevel2'.
	
	self assert: list children first children first children first children first nestedLevel equals: 3.
	self assert: list children first children first children first children first children first nestedLevel equals: 3.

]

{ #category : #tests }
MicOrderedListBlockTest >> testNotSpawingNewParagraph [
	| root list |
	root :=  parser
		parse:
'1. first item
   with a follow up 
   and another one. 
2. second item
   follow up of second item'.
	list := root children first.
	self assert: list children size equals: 2.
	self assert: list class equals: self orderedListBlockClass.
	self assert: list startIndex equals: 1. 
	self assert: list children second text equals: 'second item
follow up of second item'.
	
]

{ #category : #tests }
MicOrderedListBlockTest >> testSingleLevelList2 [
	| source root list |
	source := 
'19. item 1
    expanded
7.  item 2'.
	root := parser parse: source.
	list := root children first.
	self assert: list children size equals: 2.
	self assert: list children first text equals: 'item 1
expanded'.
	self assert: list  startIndex equals: 19
]

{ #category : #tests }
MicOrderedListBlockTest >> testSingleLevelList2WithNoIndent [
	"when we want a paragraph to be part of an item it should be indented "

	| source root list |
	source := 
'19. item 1
expanded as new paragraph
7.  item 2
    part of item2'.
	root := parser parse: source.
	self assert: root children size equals: 3.
	"two lists + one paragraph"	
	list := root children first.
	self assert: list children size equals: 1.
	self assert: list children first text equals: 'item 1'.
	self assert: list  startIndex equals: 19.
	
	self assert: root children last children first text equals: 'item 2
part of item2'
]

{ #category : #tests }
MicOrderedListBlockTest >> testSingleLevelListWithNormalNumbering [
	| root list |
	root :=  parser
		parse:
'1. first item
2. second item'.
	list := root children first.
	self assert: list children size equals: 2.
	self assert: list class equals: self orderedListBlockClass.
	self assert: list startIndex equals: 1. 
	self assert: list children second text equals: 'second item'.
	
]

{ #category : #tests }
MicOrderedListBlockTest >> testSingleLevelListWithNormalNumberingWithEmptyLines [
	| root list |
	root :=  parser
		parse:
'1. first item



2. second item'.
	self assert: root children size equals: 1.
	list := root children first.
	self assert: list children size equals: 2.
	self assert: list class equals: self orderedListBlockClass.
	self assert: list startIndex equals: 1. 
	self assert: list children second text equals: 'second item'.
	
]

{ #category : #tests }
MicOrderedListBlockTest >> testSingleLevelListWithStrangelyNumberedItems [
	| root list |
	root := parser
		parse:
			'11. item 1
2. item 2'.
	list := root children first.
	self assert: list children size equals: 2.
	self assert: list class equals: self orderedListBlockClass.
	self assert: list startIndex equals: 11. 
	self assert: list children second text equals: 'item 2'
]

{ #category : #tests }
MicOrderedListBlockTest >> testSpawingNewParagraph [
	| root |
	root :=  parser
		parse:
'1. first item
   with a follow up
a new paragraph. 
2. second list first item
   follow up of second item'.
	self assert: root children size equals: 3. 
	self assert: root children first children first text equals: 'first item
with a follow up'.
	self assert: root children third startIndex equals: 2.
	self assert: root children third children first text equals: 'second list first item
follow up of second item'.
	
]

{ #category : #tests }
MicOrderedListBlockTest >> testTwoLevelList1 [
	| source root list |
	source := 
'1. item 1
   6) sublevel
1. item 2'.
	root := parser parse: source.
	list := root children first.
	self assert: list children size equals: 2.
	self assert: list children second text equals: 'item 2'.
	self assert: list children first "item 1"
			children first "ordered list"
			children first "sub item"
			text equals: 'sublevel'
]

{ #category : #tests }
MicOrderedListBlockTest >> testedClass [ 
	^ MicOrderedListBlock 
]
