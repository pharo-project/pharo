"
I'm only for SUnit TestCases.
"
Class {
	#name : #NECTestClass,
	#superclass : #NECTestSuperClass,
	#instVars : [
		'third',
		'fourth',
		'typeSuggestingParameter',
		'messageSend',
		'messageSend2',
		'typeSuggestingParameter2',
		'complexInit',
		'complexInit2',
		'constantInteger',
		'constantSymbol',
		'constantArray',
		'constantBoolean',
		'constantString',
		'constantNil',
		'globalVarKeyword',
		'globalVarKeyword2',
		'constantLargeInteger'
	],
	#classVars : [
		'ClassVar'
	],
	#category : #'NECompletion-Tests'
}

{ #category : #initialization }
NECTestClass class >> initialize [
	super initialize.
	ClassVar := 'Any string'
]

{ #category : #initialization }
NECTestClass >> initialize: aRectangle [ 
	constantInteger := 15.
	constantLargeInteger := 1073741824.
	constantString := 'Ruben'.
	constantSymbol := #Symbol.
	constantArray := #(15 16 17 28 ).
	constantBoolean := true.
	typeSuggestingParameter := aRectangle.
	messageSend := Dictionary new.
	messageSend2 := aRectangle origin.
	globalVarKeyword := SortedCollection
				sortBlock: [:a :b | a <= b].
	globalVarKeyword2 := SortedCollection new: 15.
	constantNil := nil.
	typeSuggestingParameter2 := nil.
	complexInit := 15 > 16
				ifTrue: [#Symbol]
				ifFalse: ['String'].
	complexInit2 := Dictionary new: aRectangle origin x. 
]

{ #category : #utils }
NECTestClass >> lowPriorityOverrides: aRectangle [ 
	messageSend := aRectangle.
	typeSuggestingParameter2 := aRectangle. 
]

{ #category : #tests }
NECTestClass >> testIt: anArgument [ 
	| loc1 x2 t |
	x2 := [:bar :var | var < bar].
	loc1 := constantArray.
	x2 value: loc1.
	t := 15.
	^ x2
]
