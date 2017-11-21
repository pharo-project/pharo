"
An AmbiguousSelector is a notification produced by the Scanner/Parser/Compiler when encountering this ambiguous construct:

1@-2

Upper expression can be interpreted both
1 @ -2 (regular st-80 and former Squeak syntax, the minus is attached to the literal number)
1 @- 2 (extended binary selector, the minus sign is allowed at any position and thus part of the binary selector)

"
Class {
	#name : #AmbiguousSelectorDetected,
	#superclass : #ParserNotification,
	#instVars : [
		'interval'
	],
	#category : #Compiler-Exceptions
}

{ #category : #'instance creation' }
AmbiguousSelectorDetected class >> signalName: aString inRange: anInterval [ 
	^ (self new setName: aString range: anInterval) signal
]

{ #category : #handling }
AmbiguousSelectorDetected >> openMenuIn: aBlock [ 
	"Ask the user which selector to choose.
	Answer the choosen selector or nil if cancellation is requested."
	
	| labels actions lines caption choice |
	labels := {
		'Yes I want selector ' , name , ' with positive argument'.
		'Oops, I want ' , (name copyFrom: 1 to: name size - 1) , ' with negative argument'.
		'cancel, I must think twice'}.
	actions := {
		name.
		name copyReplaceFrom: name size to: name size - 1 with: ' '.
		nil.
	}.
	lines := {2}.
	caption := 'Ambiguous selector: ' , name , ' please correct, or cancel:'.
	choice := aBlock value: labels value: lines value: caption.
	self resume: (actions at: choice ifAbsent: [nil])
]

{ #category : #private }
AmbiguousSelectorDetected >> setName: aString range: anInterval [ 
	name := aString.
	interval := anInterval
]
