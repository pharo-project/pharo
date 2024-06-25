"
this rule verify if after a ""^"" there is a space with the return expression
"
Class {
	#name : 'ReReturnFormattingRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Formatting',
	#package : 'General-Rules',
	#tag : 'Formatting'
}

{ #category : 'accessing' }
ReReturnFormattingRule class >> group [

	^ 'Formatting'
]

{ #category : 'accessing' }
ReReturnFormattingRule class >> rationale [

	^ 'There should be a single space between the ^ and the following expression.'
]

{ #category : 'accessing' }
ReReturnFormattingRule class >> ruleName [

	^ 'Return formatting'
]

{ #category : 'helpers' }
ReReturnFormattingRule >> anchorFor: aNode [

	^ ReIntervalSourceAnchor
			entity: aNode
			interval: (Interval from: aNode start to: aNode stop)
]

{ #category : 'running' }
ReReturnFormattingRule >> basicCheck: aNode [

	^ aNode isReturn and: [ aNode start + 2 ~= aNode value start ]
]
