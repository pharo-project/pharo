Class {
	#name : #MicMathBlockTest,
	#superclass : #MicBlockTest,
	#category : #'Microdown-Tests-Parser'
}

{ #category : #tests }
MicMathBlockTest >> testMathBlockIsMathBlock [

	| root |
	root := (MicroDownParser new parse: '

&
V_i = C_0 - C_3
&


').
	self assert: root children first class equals: MicMathBlock 
]

{ #category : #tests }
MicMathBlockTest >> testMathBlockWithBadClosure [

	| root |
	"a MicMathBlock can use the MicMathInlineBlock delimiter"
	root := (MicroDownParser new parse: '

&key=cite&label=42
V_i = C_0 - C_3 
$
').
	self assert: root children first body  equals: 'V_i = C_0 - C_3 
$'.
]

{ #category : #tests }
MicMathBlockTest >> testMathBlockWithParameters [

	| root math |
	root := (MicroDownParser new parse: '

&key=cite&label=42
V_i = C_0 - C_3 
&
').
	math := root children first.
	self assert: math class equals: MicMathBlock.
	self assert: math firstLine equals: 'key=cite&label=42'.
	self assert: (math arguments at: 'key') equals: 'cite'.
	self assert: (math arguments at: 'label') equals: '42'
]

{ #category : #tests }
MicMathBlockTest >> testMathBlockWithParametersCaption [

	| root math |
	root := (MicroDownParser new parse: '

&key=cite&label=42&caption=Ze great formula
V_i = C_0 - C_3 
&
').
	math := root children first.
	self assert: math class equals: MicMathBlock.
	self assert: math firstLine equals: 'key=cite&label=42&caption=Ze great formula'.
	self assert: (math arguments at: 'key') equals: 'cite'.
	self assert: (math arguments at: 'label') equals: '42'.
	self assert: (math arguments at: 'caption') first substring equals: 'Ze great formula'.
	self assert: math caption equals: 'Ze great formula'.
]

{ #category : #tests }
MicMathBlockTest >> testMathBlockWithParametersCaptionAndFormatting [

	| root math |
	root := (MicroDownParser new parse: '

&key=cite&label=42&caption=Ze **great** formula
V_i = C_0 - C_3 
&
').
	math := root children first.
	self assert: math class equals: MicMathBlock.
	self assert: math firstLine equals: 'key=cite&label=42&caption=Ze **great** formula'.
	self assert: (math arguments at: 'key') equals: 'cite'.
	self assert: (math arguments at: 'label') equals: '42'.
	self assert: (math arguments at: 'caption') first kind equals: 'basic'.
	self assert: math caption equals: 'Ze great formula'.
	self assert: math captionElements second kind equals: 'bold' 
]

{ #category : #tests }
MicMathBlockTest >> testMathBlockWithParametersWithBlank [

	| root math |
	root := (MicroDownParser new parse: '

&key=&label=42
V_i = C_0 - C_3 
&
').
	math := root children first.
	self assert: math class equals: MicMathBlock.
	self assert: math firstLine equals: 'key=&label=42'.
	self assert: (math arguments at: 'key') equals: ''.
	self assert: (math arguments at: 'label') equals: '42'
]

{ #category : #tests }
MicMathBlockTest >> testedClass [ 
	^ MicMathBlock 
]
