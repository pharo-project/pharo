"
I simply hold the method names that have to be called before/after each other
"
Class {
	#name : #ReInvocationSequenceRuleSequence,
	#superclass : #Object,
	#instVars : [
		'before',
		'after'
	],
	#category : #Renraku-Rules
}

{ #category : #'instance creation' }
ReInvocationSequenceRuleSequence class >> before: beforeSymbol after: afterSymbol [

	^ self new
		initializeBefore: beforeSymbol
		           after: afterSymbol;
		yourself
]

{ #category : #accessing }
ReInvocationSequenceRuleSequence >> after [
	^ after
]

{ #category : #printing }
ReInvocationSequenceRuleSequence >> afterDescriptionString [

	^ self descriptionStringFor: after
]

{ #category : #accessing }
ReInvocationSequenceRuleSequence >> afterMatches: aSymbol [
	^ after includes: aSymbol
]

{ #category : #accessing }
ReInvocationSequenceRuleSequence >> before [
	^ before
]

{ #category : #printing }
ReInvocationSequenceRuleSequence >> beforeDescriptionString [

	^ self descriptionStringFor: before
]

{ #category : #accessing }
ReInvocationSequenceRuleSequence >> beforeMatches: aSymbol [
	^ before includes: aSymbol
]

{ #category : #printing }
ReInvocationSequenceRuleSequence >> descriptionStringFor: aSet [

	^ aSet size = 1
		ifTrue: [ '#', aSet anyOne, ' message' ]
		ifFalse: [ 'any one of #', (aSet joinUsing: ', #'), ' messages' ]
]

{ #category : #initialization }
ReInvocationSequenceRuleSequence >> initializeBefore: beforeSymols after: afterSymbols [

	before :=
		 (beforeSymols isCollection and:
		[ beforeSymols isString not ])
			ifFalse: [ Array with: beforeSymols asSymbol ]
			ifTrue: [ beforeSymols ].
	after := 
		(afterSymbols isCollection and:
		[ afterSymbols isString not ])
			ifFalse: [ Array with: afterSymbols asSymbol ]
			ifTrue: [ afterSymbols ].
]
