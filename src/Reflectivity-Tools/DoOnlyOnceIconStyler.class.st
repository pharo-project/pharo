"
I render an icon to control the state of #doOnlyOnce: 
"
Class {
	#name : #DoOnlyOnceIconStyler,
	#superclass : #IconStyler,
	#category : #Reflectivity-Tools-Watchpoints
}

{ #category : #defaults }
DoOnlyOnceIconStyler >> iconBlock: aNode [
	^[ :arg | self rearmOneShot]
]

{ #category : #defaults }
DoOnlyOnceIconStyler >> iconFor: aNode [
	^ self iconNamed: #smallWarningIcon 
]

{ #category : #defaults }
DoOnlyOnceIconStyler >> iconLabelBlock: aNode [

	^ [Smalltalk globals 
			at: #OneShotArmed
			ifPresent: [ :val | val ifTrue: ['active' ] ifFalse: ['disabled, click to activate' ]]
			ifAbsent: [ 'active' ]
		]
]

{ #category : #testing }
DoOnlyOnceIconStyler >> shouldStyleNode: aNode [
	^aNode isMessage and: [ aNode selector = #doOnlyOnce: ]
]
