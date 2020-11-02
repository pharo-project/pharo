Class {
	#name : #RubSelectionExtractor,
	#superclass : #Object,
	#category : #'Rubric-Tests-Base'
}

{ #category : #extraction }
RubSelectionExtractor >> extractSelectedSelectorFrom: anEditor [

	anEditor hasSelection ifFalse: [ 
		^ self findSelectorFromASTInEditor: anEditor ].

	^ self extractSelectorFromSelection: anEditor selection string
]

{ #category : #extraction }
RubSelectionExtractor >> extractSelectorFromNode: aNode [
	
	| node originalNode |
	originalNode := node := aNode.
	node isReturn ifTrue: [ 
		node := node value ].
	
	node isCascade ifTrue: [ 
		^ node messages first selector ].
	
	node isMethod ifFalse: [ 
		(node isValue and: [ node value isSymbol ]) ifTrue: [ ^ node value ].
				
		[ node isMessage or: [ node isMethod ]  ] whileFalse: [ 
	 		(node := node parent) ifNil: [ ^ nil ] ].
		
		"This is a strange case with cascades.
		Cascade nodes contain messages.
		And each message contains (duplicated) the receiver.
		So we need to deambiguate here:
		  was the selection on the receiver? or on the message itself?
		
		For example:
		  aVariable
			cascade1;
			cascade2.
			
		Selecting both the receiver or the cascade2 will yield a message node `aVariable cascade2`.
		However, in the first case we want cascade1 and in the latter we want cascade2.
		
		"
		(node ~= originalNode
			and: [ node parent notNil
				and: [ node parent isCascade ] ])
				ifTrue: [ node := node parent messages first ] ].
	
	^node selector
]

{ #category : #extracting }
RubSelectionExtractor >> extractSelectorFromSelection: aString [ 

	| node |
	node := RBParser parseFaultyExpression: aString.
	node
		nodesDo: [ :n |
			n isMessage
				ifTrue: [ ^ n selector ].
			n isVariable
				ifTrue: [ ^ n name ].
			n isLiteralNode
				ifTrue: [ ^ n value ] ].

	"fall back"
	^ aString asSymbol
]

{ #category : #extraction }
RubSelectionExtractor >> findSelectorFromASTInEditor: anEditor [

	"Try to make a selector out of the current text selection"

	| node start stop source ast originalNode |

	source := anEditor textArea string.	
	ast := anEditor editingMode parseSource: source.

	anEditor hasSelection
		ifTrue: [ self notYetImplemented ]
		ifFalse: [ | offset |
			"Heuristic: If the previous character is not a separator, take selection one position before.
			This heuristic is for the cases where the caret (|) is:
			   |self foo  => the caret is before self, do not move
			   self foo|  => the caret is before foo, interpret is as if we are in foo.
				self foo | => the caret is before a space, interpret is as if we are in foo.

			This heuristic introduces although an ambiguity when code is not nicely formatted:
			   self foo:|#bar => Here a user may want foo: or bar.
			For now we decided to favor foo: to motivate people to indent code correctly"
			offset := (source at: anEditor textArea startIndex - 1) isSeparator
				ifTrue: [ 0 ]
				ifFalse: [ -1 ].
			start := anEditor textArea startIndex + offset.
			stop := anEditor textArea stopIndex + offset ].
	node := ast bestNodeFor: (start to: stop).
	
	originalNode := node.
	node isReturn ifTrue: [ 
		node := node value ].
	
	node isCascade ifTrue: [ 
		^ node messages first selector ].
	
	node isMethod ifFalse: [ 
		(node isValue and: [ node value isSymbol ]) ifTrue: [ ^ node value ].
				
		[ node isMessage or: [ node isMethod ]  ] whileFalse: [ 
	 		(node := node parent) ifNil: [ ^ nil ] ].
		
		"This is a strange case with cascades.
		Cascade nodes contain messages.
		And each message contains (duplicated) the receiver.
		So we need to deambiguate here:
		  was the selection on the receiver? or on the message itself?
		
		For example:
		  aVariable
			cascade1;
			cascade2.
			
		Selecting both the receiver or the cascade2 will yield a message node `aVariable cascade2`.
		However, in the first case we want cascade1 and in the latter we want cascade2.
		
		"
		(node ~= originalNode
			and: [ node parent notNil
				and: [ node parent isCascade ] ])
				ifTrue: [ node := node parent messages first ] ].
	
	^node selector
]
