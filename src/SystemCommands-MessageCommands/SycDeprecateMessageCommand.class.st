"
I am a command to depracate a message selector.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newSelector:		<ByteSymbol>
"
Class {
	#name : 'SycDeprecateMessageCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
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
SycDeprecateMessageCommand >> applyResultInContext: aContext [
]

{ #category : 'accessing' }
SycDeprecateMessageCommand >> defaultMenuItemName [
	"This is not really good since this is the job of the driver to invoke the refactoring 
	but we are transitionning"
	
	^ RBDeprecateMethodTransformation menuItemString
]

{ #category : 'execution' }
SycDeprecateMessageCommand >> execute [

	(RBDeprecateMethodDriver
		 deprecateMethod: originalMessage selector
		 in: originalMessage methodClass
		 scopes: refactoringScopes) runRefactoring
]
