Class {
	#name : #GLMHighlighterTextParserStyler,
	#superclass : #SHTextStyler,
	#instVars : [
		'parser'
	],
	#category : #'Glamour-Rubric-Presentations'
}

{ #category : #accessing }
GLMHighlighterTextParserStyler >> parser [
	
	^ parser
]

{ #category : #accessing }
GLMHighlighterTextParserStyler >> parser: anObject [
	
	parser := anObject
]

{ #category : #accessing }
GLMHighlighterTextParserStyler >> privateStyle: aText [ 
	| styledText |
	aText makeAllColor: Color red.
	styledText := self parser parse: aText asString onError: [  ].
	(styledText notNil and: [ styledText ~~ aText ]) ifTrue: [ 
		self flag: 'change only the runs array of the original text.'.
		aText setString: aText string setRunsChecking: styledText runs ]
]
