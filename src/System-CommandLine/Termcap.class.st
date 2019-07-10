"
I provide a simple mapping from high-level commands to low-level terminal character sequences.

"
Class {
	#name : #Termcap,
	#superclass : #Object,
	#instVars : [
		'cache'
	],
	#category : #'System-CommandLine-Utilities'
}

{ #category : #initialization }
Termcap >> flush [
	cache removeAll
]

{ #category : #accessing }
Termcap >> getent: aString [
	"The tgetent routine loads the entry for name. 
	It returns 1 on success, 
	0 if there is no such entry,
	 and -1 if the terminfo database could not be found."
	
	"result := (PipeableOSProcess command: 'tput ', aString) output asInteger.
	result = 1 ifTrue: [ ^ self ].
	result = 0 ifTrue: [ Error signal: 'No such tget entry: ', aString aString ].
	result = -1 ifTrue: [ Error signal: 'Could not find terminfo database' ]."
	
	Error signal: 'Cannot interact with tput yet'.
	
]

{ #category : #accessing }
Termcap >> getflag: aString [
	"^ (PipeableOSProcess command: 'tput ', aString) output"
	Error signal: 'Cannot interact with tput yet'
]

{ #category : #accessing }
Termcap >> getnum: aString [
	"^ (PipeableOSProcess command: 'tput ', aString) output asInteger"
	^ Error signal: 'Cannot interact with the terminal yet'.
]

{ #category : #accessing }
Termcap >> getstr: aString [
	^ cache
		at: aString 
		ifAbsentPut: [ 
			"(PipeableOSProcess command: 'tput ', aString) output"
			^ Error signal: 'Terminal capability ', aString, ' not found' ]
]

{ #category : #initialization }
Termcap >> initialize [
	cache := Dictionary new.
	self initializeDefaultSequences.
]

{ #category : #initialization }
Termcap >> initializeDefaultSequences [
	"Until we have decent OSProcess support we precache the most common sequences"
	| esc |
	esc := (Character escape asString), '['.
	cache at: #up put: (esc, 'A').
	cache at: #le put: (esc, 'D').
	cache at: #nd put: (esc, 'C').
	cache at: #im put: ''.
	cache at: #ei put: ''.
	cache at: #mr put: (esc, '7m').
	cache at: #me put: (esc, '0m').
	cache at: #ce put: (esc, 'K').
	cache at: #al put: (esc, 'L').
	cache at: #dl put: (esc, 'M').
	cache at: #cr put: (esc, String cr).
]

{ #category : #initialization }
Termcap >> reset [
	self flag: 'TODO implement once we have OSProcess'.
	"self getent: (OSProcess thisOSProcess environmentAt: #TERM)"
]
