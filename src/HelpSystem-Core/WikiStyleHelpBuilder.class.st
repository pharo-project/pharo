"
I'm an HelpSystem builder which create an HelpSystem book from a wiki-like formatted string. 

! Usage

- Create a class-side method on the class you want to put documentation.
- Add the pragma <wikiStyleHelp:> with the title of the book as parameter
- The method should answer the documentation as a String (using Pier syntax. See http://www.piercms.com/doc/syntax ). 

! Example

MyClass class>>aMethod
        <wikiStyleHelp: #'The name of the book I want to write'>
        ^ '
 ! Section 1

 Some text for this section

 !! Subsection 1.1
 !!! Subsection 1.1.1
 !! Subsection 1.2
 ! Section 2'
"
Class {
	#name : #WikiStyleHelpBuilder,
	#superclass : #HelpBuilder,
	#category : #'HelpSystem-Core-Builders'
}

{ #category : #'private accessing' }
WikiStyleHelpBuilder class >> allHelpPragmas [
	 ^(PragmaCollector filter: [:pragma | self pragmaKeywords includes: pragma selector]) reset collected

]

{ #category : #help }
WikiStyleHelpBuilder class >> help [
        <wikiStyleHelp: #'Write documentation with wiki like syntax'>
        ^ 'From WikiStyleHelpBuilder comment: ', String cr, self comment
]

{ #category : #'private accessing' }
WikiStyleHelpBuilder class >> pragmaKeyword [
	 ^#wikiStyleHelp:

]

{ #category : #'private accessing' }
WikiStyleHelpBuilder class >> pragmaKeywords [
	 ^#(wikiStyleHelp: wikiStyleHelp:syntax:)

]

{ #category : #building }
WikiStyleHelpBuilder >> build [
	| text extract topic |
	topicToBuild title: 'Wiki style help' translated.
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

{ #category : #'private accessing' }
WikiStyleHelpBuilder >> defaultSyntax [
	^#pier
]

{ #category : #'private extraction' }
WikiStyleHelpBuilder >> extractHelpContentFromString: aString [
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

{ #category : #'private accessing' }
WikiStyleHelpBuilder >> helpContentFromString: aString [
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

{ #category : #'private extraction' }
WikiStyleHelpBuilder >> helpStartMarker [
	^  '<', self helpTag, '>'
]

{ #category : #'private extraction' }
WikiStyleHelpBuilder >> helpStopMarker [
	^  '</', self helpTag, '>'
]

{ #category : #'private extraction' }
WikiStyleHelpBuilder >> helpTag [
	^  #help
]

{ #category : #'private conversion' }
WikiStyleHelpBuilder >> helpTopicFromFormattedString: aString title: aTitle [
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

{ #category : #'private accessing' }
WikiStyleHelpBuilder >> sectionStartChar [
	^ $!
]

{ #category : #'private accessing' }
WikiStyleHelpBuilder >> supportedSyntaxes [
	^#(pier) 
]
