"
I am a specialized layout which does not hold slots but only raw data (bytes or words).
"
Class {
	#name : #BitsLayout,
	#superclass : #ObjectLayout,
	#category : 'Slot-Layout'
}

{ #category : #extending }
BitsLayout >> extend [
	"Answer a default extension of me."

	^ self species new 
]

{ #category : #reshaping }
BitsLayout >> extendAgain: aLayout with: aScope [ 
	"Answer my default layout since any subclass of me only can have my layout type."

	^ self extend
		host: host;
		yourself
]

{ #category : #extending }
BitsLayout >> extendWeak: aLayoutClassScope [ 
	IncompatibleLayoutConflict new
		layout: self;
		subType: #weak;
		signal
]

{ #category : #initialize-release }
BitsLayout >> initializeInstance: anInstance [
]

{ #category : #testing }
BitsLayout >> isBits [
	^ true
]

{ #category : #testing }
BitsLayout >> isBytes [
	^ self subclassResponsibility
]

{ #category : #testing }
BitsLayout >> isVariable [
	^ true
]

{ #category : #testing }
BitsLayout >> isWords [
	^ self isBytes not
]

{ #category : #reshaping }
BitsLayout >> reshapeFrom: oldScope to: newParentLayout [
	"Answer the reshaped version of me for a new parent layout.

	If the new parent layout has not fields, then the new version 
	is just a copy of me. But if it has fields, that means that 
	the new version of me cannot be a bits layout anymore, so the
	answer is a new default layout. "

	^ newParentLayout hasFields 
		ifTrue: [ 
			(newParentLayout extend)
				host: host;
				yourself ]
		ifFalse: [ 
			self copy ]

]
