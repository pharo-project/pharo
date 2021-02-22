"
I am a command to rename message selector in specific packages.
In addition I allow to swap arguments in signature.
 
Internal Representation and Key Implementation Points.

Instance Variables
	newSignature:		<SycMessageDescription>
	packages:          <OrderedCollection>
"
Class {
	#name : #SycRenameMessageInSomePackagesCommand,
	#superclass : #SycRenameMessageCommand,
	#instVars : [
		'packages'
	],
	#category : #'SystemCommands-MessageCommands'
}

{ #category : #execution }
SycRenameMessageInSomePackagesCommand >> createRefactoring [
	|refactoring|
	refactoring := RBRenameMethodRefactoring
		renameMethod: originalMessage selector 
		in: (SycMessageOriginHack of: originalMessage) "look at SycMessageOriginHack comment"
		to: newSignature selector 
		permutation: (originalMessage computeArgumentPermutationsFor: newSignature).
	refactoring searchInPackages: packages.
	^ refactoring
]

{ #category : #accessing }
SycRenameMessageInSomePackagesCommand >> defaultMenuItemName [
	^'Rename in specific packages'
]

{ #category : #execution }
SycRenameMessageInSomePackagesCommand >> prepareFullExecutionInContext: aToolContext [
	super prepareFullExecutionInContext: aToolContext.
	
	packages := (aToolContext requestMultiplePackages: 'Choose packages') collect: [ :e | e name ]
]
