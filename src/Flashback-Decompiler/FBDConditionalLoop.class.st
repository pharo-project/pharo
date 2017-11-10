"
I represent a conditional loop (typically, a for or while loop). In addition to my superclass I hold the pc of the conditional jump that exits the loop body.

exitCondition <Smi> pc of the conditional jump where the execution flows exits the loop body.
"
Class {
	#name : #FBDConditionalLoop,
	#superclass : #FBDLoop,
	#instVars : [
		'exitCondition'
	],
	#category : #Flashback-Decompiler
}

{ #category : #accessing }
FBDConditionalLoop >> exitCondition [
	^ exitCondition
]

{ #category : #accessing }
FBDConditionalLoop >> exitCondition: anObject [
	exitCondition := anObject
]

{ #category : #testing }
FBDConditionalLoop >> isConditionalLoop [
	^ true
]
