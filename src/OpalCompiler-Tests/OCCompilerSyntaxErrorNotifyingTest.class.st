"
A CompilerSyntaxErrorNotifyingTest is a specialization for testing correct handling of non interactive compiler notification.
Non interactive is a very relative notion in Smalltalk...
Here it means that user interaction will not happen directly in the TextEditor holding source code, but rather thru a SyntaxError window that will pop-up.
This test intercept the Notification before the pop-up is raised.

"
Class {
	#name : #OCCompilerSyntaxErrorNotifyingTest,
	#superclass : #OCCompilerNotifyingTest,
	#category : #OpalCompiler-Tests-FromOld
}

{ #category : #testing }
OCCompilerSyntaxErrorNotifyingTest class >> shouldInheritSelectors [
	"This class can recycle all of super tests, it just has to refine internal Compiler evaluation machinery"
	^true
]

{ #category : #private }
OCCompilerSyntaxErrorNotifyingTest >> enumerateAllSelections [
	"This method intercepts the SyntaxErrorNotification and prevent the SyntaxError morph to open.
	The notification errorCode hold the source of evaluated sub-selection with inserted error message.
	This can be compared to expected error notification."
	1 to: self numberOfSelections do: [:n |
		| result |
		result := [self evaluateSelectionNumber: n] on: SyntaxErrorNotification do: [:exc |
			| expectedNotification expectedNotificationLocation |
			expectedNotification := (expectedErrors at: n) allButFirst allButLast: 3.
			expectedNotificationLocation := (expectedErrorPositions at: n) - (morph editor startIndex - 1).
			self assert: expectedNotificationLocation equals: exc location.
			self assert: expectedNotification equals: exc errorMessage asString.
			exc return: nil]].
]

{ #category : #private }
OCCompilerSyntaxErrorNotifyingTest >> evaluateSelection [
	^ OpalCompiler new
		source: morph editor selection readStream;
		"Note subtle difference versus  (morph editor selectionAsStream). 
		The later does not answer the same contents and would raise a SyntaxErrorNotification with wrong sub-selection"
		failBlock: [^failure];
		evaluate
]
