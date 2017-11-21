"
Weak action sequences for events
"
Class {
	#name : #WeakActionSequence,
	#superclass : #Array,
	#type : #variable,
	#category : #'System-Object Events'
}

{ #category : #converting }
WeakActionSequence >> asActionSequence [

	^self
]

{ #category : #converting }
WeakActionSequence >> asMinimalRepresentation [

	| valid |
	valid := self select: [:e | e isValid ].
	valid size = 0
		ifTrue: [^nil].
	valid size = 1
		ifTrue: [^valid first].
	^valid
]

{ #category : #printing }
WeakActionSequence >> printOn: aStream [

	self size < 2 ifTrue: [^super printOn: aStream].
	aStream nextPutAll: '#('.
	self
		do: [:each | each printOn: aStream]
		separatedBy: [aStream cr].
	aStream nextPut: $)
]

{ #category : #evaluating }
WeakActionSequence >> value [
    "Answer the result of evaluating the elements of the receiver.
	Actually, return just the last result."

    | answer |
    self do:
        [:each | each isValid ifTrue: [answer := each value]].
    ^answer
]

{ #category : #evaluating }
WeakActionSequence >> valueWithArguments: anArray [

	"Return the last result"

    | answer |
    self do:
        [:each |
        	each isValid ifTrue: [answer := each valueWithArguments: anArray]].
    ^answer
]
