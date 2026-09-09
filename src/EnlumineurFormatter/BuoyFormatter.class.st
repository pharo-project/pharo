"
I'm a code formatter implementing the ba-st conventions
"
Class {
	#name : 'BuoyFormatter',
	#superclass : 'EFFormatter',
	#category : 'EnlumineurFormatter',
	#package : 'EnlumineurFormatter'
}

{ #category : 'private - formatting' }
BuoyFormatter >> formatArray: anArrayNode [

  codeStream nextPutAll: self spacesInsideArray.
  self indentAround: [ self formatArrayBody: anArrayNode ].
  codeStream nextPutAll: self spacesInsideArray
]

{ #category : 'private - formatting' }
BuoyFormatter >> formatArrayBody: aSequenceNode [

  | statements currentStatement |
  statements := aSequenceNode statements.
  statements isEmpty ifTrue: [ ^ self ].
  statements
    do: [ :statement |
        self visitNode: statement.
        self formatStatementCommentsFor: statement.
        currentStatement := statement
      ]
    separatedBy: [
        codeStream
          nextPutAll: self spacesBeforeDotInDynamicArray;
          nextPut: $..
        ( self isLineTooLongWithNode: currentStatement )
          ifTrue: [ self newLine ]
          ifFalse: [ self space ]
      ]
]

{ #category : 'private - formatting' }
BuoyFormatter >> formatBlock: aBlockNode [

  | isMultiline isBodyTooLong |
  isMultiline := self willBeMultiline: aBlockNode body.

  codeStream nextPutAll: self spacesInsideBlocksString.

  self formatBlockArgumentsFor: aBlockNode.
  self formatBlockCommentFor: aBlockNode.

  isBodyTooLong := self isLineTooLongWithNode: aBlockNode body.

  ( isMultiline or: [ isBodyTooLong ] )
    ifTrue: [
        self indentAround: [
            ( self shouldPassNewLineAfterHeadOfBlock: aBlockNode ) ifTrue: [ self newLine ].
            self visitSequenceNode: aBlockNode body
          ]
      ]
    ifFalse: [ self visitSequenceNode: aBlockNode body ].

  ( self lineUpBlockBrackets and: [ isMultiline ] )
    ifTrue: [ self newLine ]
    ifFalse: [ codeStream nextPutAll: self spacesInsideBlocksString ]
]

{ #category : 'private' }
BuoyFormatter >> needsParenthesisFor: aNode [

  | parent |
  aNode ifNil: [ ^ false ].
  aNode isValue ifFalse: [ ^ false ].
  aNode isParseError ifTrue: [ ^ aNode hasParentheses ].
  parent := aNode parent ifNil: [ ^ false ].
  ^ parent isDynamicArray
      ifTrue: [ false ]
      ifFalse: [ super needsParenthesisFor: aNode ]
]
