"
This rule verify if there is spaces inside parentheses
"
Class {
	#name : 'ReParensFormattingRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Formatting',
	#package : 'General-Rules',
	#tag : 'Formatting'
}

{ #category : 'accessing' }
ReParensFormattingRule class >> group [

	^ 'Formatting'
]

{ #category : 'accessing' }
ReParensFormattingRule class >> rationale [

	^ 'There should be no spaces inside parentheses.'
]

{ #category : 'accessing' }
ReParensFormattingRule class >> ruleName [

	^ 'Parentheses formatting'
]

{ #category : 'helpers' }
ReParensFormattingRule >> anchorFor: aNode [

	^ ReIntervalSourceAnchor
			entity: aNode
			interval: (Interval from: aNode start to: aNode stop)
]

{ #category : 'running' }
ReParensFormattingRule >> basicCheck: aNode [

	| found start stop |
	^ aNode isValue and: [
		  aNode hasParentheses and: [
			  found := false.
			  start := aNode startWithoutParentheses.
			  stop := aNode stopWithoutParentheses.
			  aNode parentheses reverseDo: [ :interval |
				  found := found or: [ start - 1 ~= interval first or: [ stop + 1 ~= interval last ] ].
				  start := interval first.
				  stop := interval last ].
			  found ] ]
]
