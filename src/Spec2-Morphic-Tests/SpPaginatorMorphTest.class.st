Class {
	#name : #SpPaginatorMorphTest,
	#superclass : #TestCase,
	#instVars : [
		'morph'
	],
	#category : #'Spec2-Morphic-Tests'
}

{ #category : #running }
SpPaginatorMorphTest >> setUp [

	super setUp.
	morph := SpPaginatorMorph new 
		color: Color white;
		hResizing: #shrinkWrap;
		yourself.
	1 to: 10 do: [ :index | 
		morph addPage: index ].
	morph openInWorld.

]

{ #category : #running }
SpPaginatorMorphTest >> tearDown [

	morph delete.
	super tearDown
]

{ #category : #tests }
SpPaginatorMorphTest >> testSelectPage [

	self assert: morph selectedPages equals: #(1 2).
	morph selectPage: 5.
	self assert: morph selectedPages equals: #(5 6).
	"10 is last, I cannot select 10, 11... then I select 9, 10"
	morph selectPage: 10.
	self assert: morph selectedPages equals: #(9 10)
]

{ #category : #tests }
SpPaginatorMorphTest >> testWhenNumberOfPagesShownChangedDo [
	| numberShown |

	morph whenNumberOfPagesShownChangedDo: [ :aNumber | numberShown := aNumber ].
	morph selectPage: 5.
	morph numberOfPagesShown: 3.

	self assert: numberShown equals: 3
]

{ #category : #tests }
SpPaginatorMorphTest >> testWhenSelectedPageChangedDo [
	| selectedPage |

	morph whenSelectedPageChangedDo: [ :pageNumber | selectedPage := pageNumber ].
	morph selectPage: 5.

	self assert: selectedPage equals: 5	
]
