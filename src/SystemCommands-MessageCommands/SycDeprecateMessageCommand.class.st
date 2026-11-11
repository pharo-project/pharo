"
I am a command to depracate a message selector.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newSelector:		<ByteSymbol>
"
Class {
	#name : 'SycDeprecateMessageCommand',
	#superclass : 'SycChangeMessageMenuCmCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}

{ #category : 'accessing' }
SycDeprecateMessageCommand class >> browserMenuOrder [
	^ 2.5
]

{ #category : 'testing' }
SycDeprecateMessageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : 'execution' }
SycDeprecateMessageCommand >> applyCommandResult [
]

{ #category : 'execution' }
SycDeprecateMessageCommand >> executeRefactoring [

	(RBDeprecateMethodDriver
		 deprecateMethod: originalMessage selector
		 in: originalMessage methodClass
		 scopes: refactoringScopes) runRefactoring
]

{ #category : 'accessing' }
SycDeprecateMessageCommand >> name [
	"This is not really good since this is the job of the driver to invoke the refactoring 
	but we are transitionning"
	
	^ RBDeprecateMethodTransformation menuItemString
]

{ #category : 'accessing' }
SycDeprecateMessageCommand >> order [

	^ 2.5
]

{ #category : 'accessing' }
SycDeprecateMessageCommand >> orderInContext: aToolContext [

	^ aToolContext isMethodSourceContext
		ifTrue: [ 1.3 ]
		ifFalse: [ self order ]
]
