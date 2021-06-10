"
I'm an HelpSystem builder which creates an HelpSystem book from a microdown formatted string. 

# Usage

- Create a class-side method on the class you want to put documentation.
- Add the pragma <microdownStyleHelp:> with the title of the book as parameter
- The method should answer the documentation as a String using microdown (see the help syntax on class comment editor).

"
Class {
	#name : #MicrodownStyleHelpBuilder,
	#superclass : #HelpBuilder,
	#category : #'HelpSystem-Core-Builders'
}

{ #category : #'private - accessing' }
MicrodownStyleHelpBuilder class >> allHelpPragmas [
	^ self pragmaKeywords flatCollect: [ :keyword | Pragma allNamed: keyword ]
]

{ #category : #help }
MicrodownStyleHelpBuilder class >> help [
	<microdownStyleHelp: #'Write documentation with Microdown'>
	^ self comment
]

{ #category : #'private - accessing' }
MicrodownStyleHelpBuilder class >> pragmaKeyword [
	 ^ #microdownStyleHelp:

]

{ #category : #'private - accessing' }
MicrodownStyleHelpBuilder class >> pragmaKeywords [
	 ^ #(microdownStyleHelp:)

]

{ #category : #building }
MicrodownStyleHelpBuilder >> build [
	| text extract topic |
	topicToBuild title: 'Midrodown style help' translated.
	self class allHelpPragmas
		do: [ :each | 
			"Its assumed that if the pragma is a single keyword only the title is given
		 and the default syntax applies, otherwise the syntax is given as last parameter
		 on the pragmas keyword message"
			text := each method methodClass instanceSide perform: each method selector.
			extract := self extractHelpContentFromString: text.
			topic := self helpTopicFromFormattedString: extract title: each arguments first.
			topicToBuild addSubtopic: topic ]
]

{ #category : #'private - accessing' }
MicrodownStyleHelpBuilder >> defaultSyntax [
	^#microdown
]

{ #category : #'private - extraction' }
MicrodownStyleHelpBuilder >> extractHelpContentFromString: aString [
	| start stop |
	start := aString findString: self helpStartMarker.
	start := start = 0
		ifTrue: [ 1 ]
		ifFalse: [ start + self helpStartMarker size ].
	stop := aString findString: self helpStopMarker startingAt: start.
	stop := stop = 0
		ifTrue: [ aString size ]
		ifFalse: [ stop - 1 ].
	^ (aString copyFrom: start to: stop) trimBoth
]

{ #category : #'private - extraction' }
MicrodownStyleHelpBuilder >> helpStartMarker [
	^  '<', self helpTag, '>'
]

{ #category : #'private - extraction' }
MicrodownStyleHelpBuilder >> helpStopMarker [
	^  '</', self helpTag, '>'
]

{ #category : #'private - extraction' }
MicrodownStyleHelpBuilder >> helpTag [
	^  #help
]

{ #category : #'private - conversion' }
MicrodownStyleHelpBuilder >> helpTopicFromFormattedString: aString title: aTitle [
	| helpString currentLevel section topicStack topic |
	helpString := aString.
	topicStack := Stack new.
	topicStack push: (HelpTopic new title: aTitle).
	currentLevel := 0.
	section := ''.
	helpString 
		linesDo: [:line | 
			(line beginsWith: self sectionStartChar asString)
				ifTrue: [ |  level title |					
					section ~= ''
						ifTrue: [topicStack top contents: section trimLeft.
							section := ''].
					level := 0.
					[(level < line size) and: [(line at: level + 1) = self sectionStartChar]]
						whileTrue: [level := level + 1].
					title := (line copyFrom: level + 1 to: line size) trimLeft.
					level > currentLevel 
						ifTrue: [(level - currentLevel)
							timesRepeat: [topicStack push:  HelpTopic new].
							topicStack top title: title]
						ifFalse: [currentLevel = level
							ifTrue: [topic := topicStack pop.
								topicStack top addSubtopic: topic.
								topicStack push: (HelpTopic new title: title)]
							ifFalse: [
								(currentLevel - level) + 1 
									timesRepeat: [topic := topicStack pop.
										topicStack top addSubtopic: topic].
								topicStack push: (HelpTopic new title: title)]].
					currentLevel := level]
				ifFalse: [section := section, String cr, line]].
	section ~= ''
		ifTrue: [topicStack top contents: section trimLeft].
	[topicStack isEmpty]
		whileFalse: [topic := topicStack pop.
			topicStack isEmpty ifFalse: [topicStack top addSubtopic: topic]].
	^ topic

]

{ #category : #'private - accessing' }
MicrodownStyleHelpBuilder >> sectionStartChar [
	^ $!
]

{ #category : #'private - accessing' }
MicrodownStyleHelpBuilder >> supportedSyntaxes [
	^#(microdown) 
]
