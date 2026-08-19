"
I check that the method to be inlined does not send super messages
that resolve differently in the calling context than in the original
class hierarchy.

If such a mismatch exists, inlining would silently change which method
is dispatched for those super sends.
"
Class {
	#name : 'ReInlineMethodSuperMessagesCondition',
	#superclass : 'ReMethodsCondition',
	#instVars : [
		'class',
		'callerClass',
		'inlineParseTree'
	],
	#category : 'Refactoring-Conditions',
	#package : 'Refactoring-Conditions'
}

{ #category : 'instance creation' }
ReInlineMethodSuperMessagesCondition >> class: aClassToInline callerClass: aCallerClass inlineParseTree: aParseTree [

    class := aClassToInline.
    callerClass   := aCallerClass.
    inlineParseTree := aParseTree
]

{ #category : 'displaying' }
ReInlineMethodSuperMessagesCondition >> violationMessageOn: aStream [

	aStream nextPutAll: 'Cannot inline method since it sends super messages that resolve differently in the calling context:'.
	self violators do: [ :each |
			aStream
				nextPutAll: '  - ';
				nextPutAll: each ]
]

{ #category : 'accessing' }
ReInlineMethodSuperMessagesCondition >> violators [

	violators ifNotNil: [ ^ violators ].
	violators := OrderedCollection new.

	class = callerClass ifTrue: [ ^ violators ].
	class superclass ifNil: [ ^ violators ].

	violators addAll: (inlineParseTree superMessages reject: [ :each |
			 (class superclass whichClassIncludesSelector: each) = (callerClass superclass whichClassIncludesSelector: each) ]).

	^ violators
]
