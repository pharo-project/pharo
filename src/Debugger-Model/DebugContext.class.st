"
A DebugContext is a helper that complements DebugSession. It is meant to be created dynamically on a context when the session wants to access the provided services.

To create instances first call forContext: to set the current context, and then if
the interrupted is different use topContext:

Not sure if it is still a good idea to have this class. 

Instance Variables
	context:		<Object>
	method:		<Object>
	methodNode:		<Object>
	ranges:		<Object>
	topContext:		<Object>

context
	- xxxxx

method
	- xxxxx

methodNode
	- xxxxx

ranges
	- xxxxx

topContext
	- xxxxx

"
Class {
	#name : 'DebugContext',
	#superclass : 'Object',
	#instVars : [
		'context'
	],
	#category : 'Debugger-Model-Core',
	#package : 'Debugger-Model',
	#tag : 'Core'
}

{ #category : 'instance creation' }
DebugContext class >> forContext: aContext [
	^ self new forContext: aContext
]

{ #category : 'private' }
DebugContext >> blockNotFoundDialog: aMethod with: aText [

	| message result newMethod |
	message := 'Method for block not found on stack, can''t edit and continue'.

	"shouldn't edit doits"
	aMethod selector isDoIt ifTrue: [ ^ self inform: message ].

	result := self
		          confirm: message
		          trueChoice: 'Compile and Browse'
		          falseChoice: 'Cancel'.

	"possible return values are true | false | nil"
	result ifFalse: [ ^ self ].

	newMethod := self recompileCurrentMethodTo: aText notifying: nil.
	newMethod ifNotNil: [ self browseMethod: newMethod ]
]

{ #category : 'private' }
DebugContext >> browseMethod: aMethod [

	aMethod browse
]

{ #category : 'private' }
DebugContext >> checkSelectorUnchanged: aSelector [
	| unchanged |
	unchanged := aSelector == self selectedMessageName
		or: [ self selectedMessageName isDoIt and: [ aSelector numArgs = self selectedMessageName numArgs ] ].
	unchanged
		ifFalse: [ self inform: 'can''t change selector' ].
	^ unchanged
]

{ #category : 'ui requests' }
DebugContext >> confirm: message trueChoice: trueChoice falseChoice: falseChoice [

	^ UIManager default
		confirm: message
		trueChoice: trueChoice
		falseChoice: falseChoice
]

{ #category : 'private' }
DebugContext >> confirmOnTraitOverwrite: aSelector inClass: aClass [
	"test if method originates from a trait and let the user choose which one should be change"

	| target method |
	target := self selectedClass.
	method := aClass methodNamed: aSelector.
	method isFromTrait ifTrue: [ |trait|
		trait := method origin.
		target := UIManager default
			chooseFrom: {('Create copy in ' , aClass name).
			('Compile Trait method in ' , trait name)}
			values: {aClass. trait }
			title: 'Where do you want to compile this trait method?' ].
	^ target
]

{ #category : 'accessing' }
DebugContext >> context [
	^ context
]

{ #category : 'evaluating actions' }
DebugContext >> evaluate: expression [
	^ context compiler evaluate: expression
]

{ #category : 'initialization' }
DebugContext >> forContext: aContext [
	context := aContext
]

{ #category : 'accessing' }
DebugContext >> locateClosureHomeWithContent: aText [
	"In case the current context is a BlockContext locate the closureHome and ask the
	user to validate the new context. If closureHome is not found or the user does not
	validate the new context, return nil. aText is the new content of the current context.
	If the current context is not a BlockContext return it."
	| closureHome |

	context isBlockContext ifTrue: [
		closureHome := context activeHome.
		closureHome ifNil: [
			self blockNotFoundDialog: context homeMethod with: aText.
		 	^ nil ].
		 (self confirm: 'I will have to revert to the method from\which this block originated.  Is that OK?' withCRs)
			ifFalse: [ ^ nil ].
		^ closureHome].

	^ context
]

{ #category : 'accessing' }
DebugContext >> receiver [
	^ context receiver
]

{ #category : 'accessing' }
DebugContext >> receiverClass [
	"Answer the class of the receiver.
	It may differ from 'self selectedClass' "

	^ context receiver class
]

{ #category : 'evaluating actions' }
DebugContext >> recompileCurrentMethodTo: aText notifying: aNotifyer [

	| classOrTraitOfMethod selector |
	self context method isDoIt ifFalse: [
		selector := self selectedClass compiler parseSelector: aText.
		(self checkSelectorUnchanged: selector) ifFalse: [ ^ nil ] ].

	(self selectedClass includesSelector: selector) ifFalse: [
		^ self selectedClass compiler
			  protocol: self selectedMessageCategoryName;
			  isScripting: self selectedMessageName isDoIt;
			  requestor: aNotifyer;
			  compile: aText ].

	classOrTraitOfMethod := self
		                        confirmOnTraitOverwrite: selector
		                        inClass: self selectedClass.
	classOrTraitOfMethod ifNil: [ ^ nil ].

	^ classOrTraitOfMethod compiler
		  protocol: self selectedMessageCategoryName;
		  isScripting: self selectedMessageName isDoIt;
		  requestor: aNotifyer;
		  install: aText
]

{ #category : 'accessing' }
DebugContext >> selectedClass [
	"Answer the class in which the current context's method was found."

	^ context methodClass
]

{ #category : 'accessing' }
DebugContext >> selectedMessageCategoryName [
	"Answer the name of the message category of the message of the current context."

	^ self selectedClass protocolNameOfSelector: self selectedMessageName
]

{ #category : 'accessing' }
DebugContext >> selectedMessageName [
	"Answer the message selector of the current context.
	 If the method is unbound we can still usefully answer its old selector."

	^ context selector
]

{ #category : 'private' }
DebugContext >> source [
	"Answer the source code of the currently selected context."
	^ context sourceCode
]
