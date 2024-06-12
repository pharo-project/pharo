"
This rule verify if there is spaces at the beginning and end of block. Otherwise, she is trigerred.
"
Class {
	#name : 'ReBlockFormattingRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Formatting',
	#package : 'General-Rules',
	#tag : 'Formatting'
}

{ #category : 'accessing' }
ReBlockFormattingRule class >> group [

	^ 'Formatting'
]

{ #category : 'accessing' }
ReBlockFormattingRule class >> rationale [
	
	^ 'There should spaces at the beginning and end of block.'
]

{ #category : 'accessing' }
ReBlockFormattingRule class >> ruleName [

	^ 'Block formatting'
]

{ #category : 'helpers' }
ReBlockFormattingRule >> anchorFor: aNode [

	^ ReIntervalSourceAnchor
			entity: aNode
			interval: (Interval from: aNode start to: aNode stop)
]

{ #category : 'running' }
ReBlockFormattingRule >> basicCheck: anEntity [

	| source before after |
	anEntity isBlock ifFalse: [ ^ false ].
	
	source := anEntity source.
	(source notNil and: [ anEntity left notNil and: [ anEntity right notNil ] ]) ifFalse:[ ^ false ].
	
	before := source at: anEntity left + 1 ifAbsent: [ $  ].
	after := source at: anEntity right - 1 ifAbsent: [ $  ].
	(before isSeparator and: [ after isSeparator ])
		ifFalse: [ ^ true ]
		ifTrue: [
			anEntity bar ifNotNil: [
				before := source at: anEntity bar - 1 ifAbsent: [ $  ].
				after := source at: anEntity bar + 1 ifAbsent: [ $  ].
				(before isSeparator and: [ after isSeparator ]) ifFalse: [ ^ true ] ] ].
	^ false
]
