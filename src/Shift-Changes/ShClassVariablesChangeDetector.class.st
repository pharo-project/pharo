"
I know how to detect a change in the collection of the class variables
"
Class {
	#name : #ShClassVariablesChangeDetector,
	#superclass : #ShAbstractClassChangeDetector,
	#category : #'Shift-Changes'
}

{ #category : #initialization }
ShClassVariablesChangeDetector >> compareFrom: a to: b [	
	^ a asDictionary = b asDictionary.
]

{ #category : #initialization }
ShClassVariablesChangeDetector >> initialize [
	super initialize.
	builderAccessor := [ :e | e layoutDefinition classVariables collect:[ :x | x key -> x class ] as: Array ].
	classAccessor := [ :e | e classVariables collect: [ :x | x key -> x class ] as: Array ].
	comparer := [ :a :b | self compareFrom: a to: b ]
]
