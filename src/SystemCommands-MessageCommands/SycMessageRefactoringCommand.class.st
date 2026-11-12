"
I am the root of the refactorings which change the senders or the signature of the selected message: add or remove an argument, find and replace, inline senders, remove all senders, replace senders.
I keep the dialogs and the refactoring support of SycChangeMessageSignatureCommand.
In method editors my commands are placed near the top of the menu (#orderInContext:).
"
Class {
	#name : 'SycMessageRefactoringCommand',
	#superclass : 'SycChangeMessageSignatureCommand',
	#category : 'SystemCommands-MessageCommands',
	#package : 'SystemCommands-MessageCommands'
}
