Class {
	#name : 'RBParseTreeEnvironment',
	#superclass : 'RBSelectorEnvironment',
	#instVars : [
		'matcher'
	],
	#category : 'Refactoring-Critics',
	#package : 'Refactoring-Critics'
}

{ #category : 'initialization' }
RBParseTreeEnvironment >> matcher: aParseTreeSearcher [
	matcher := aParseTreeSearcher
]

{ #category : 'accessing' }
RBParseTreeEnvironment >> selectionIntervalFor: aString [

	| parseTree node |

	matcher ifNil: [ ^ super selectionIntervalFor: aString ].
	parseTree := RBParser
		parseMethod: aString
		onError: [ :error :position | ^ super selectionIntervalFor: aString ].
	node := matcher executeTree: parseTree initialAnswer: nil.
	^ ( node isKindOf: ASTProgramNode )
		ifTrue: [ node sourceInterval ]
		ifFalse: [ super selectionIntervalFor: aString ]
]
