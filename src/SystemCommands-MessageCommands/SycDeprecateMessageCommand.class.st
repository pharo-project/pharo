"
I am a command to depracate a message selector.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	newSelector:		<ByteSymbol>
"
Class {
	#name : #SycDeprecateMessageCommand,
	#superclass : #SycChangeMessageSignatureCommand,
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #accessing }
SycDeprecateMessageCommand class >> browserMenuOrder [
	^ 2.5
]

{ #category : #testing }
SycDeprecateMessageCommand class >> canBeExecutedInContext: aToolContext [
	^ aToolContext isMethodSelected
]

{ #category : #execution }
SycDeprecateMessageCommand >> applyResultInContext: aContext [
]

{ #category : #accessing }
SycDeprecateMessageCommand >> defaultMenuItemName [

	^ 'Deprecate'
]

{ #category : #execution }
SycDeprecateMessageCommand >> execute [

	(RBDeprecateMethodDriver
		 deprecateMethod: originalMessage selector
		 in: originalMessage methodClass
		 scopes: refactoringScopes) execute
]

{ #category : #testing }
SycDeprecateMessageCommand >> isComplexRefactoring [
	"We need to override this to prevent `RBRefactoringPreviewPresenter` from opening (See `CmdCommand >> execute` for details.)
	We want this command to call the appropriate Driver that will execute refactoring."

	^ false
]
