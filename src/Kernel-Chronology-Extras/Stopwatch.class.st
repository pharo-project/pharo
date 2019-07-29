"
A Stopwatch maintains a collection of timespans.
"
Class {
	#name : #Stopwatch,
	#superclass : #Object,
	#instVars : [
		'timespans',
		'state'
	],
	#category : #'Kernel-Chronology-Extras'
}

{ #category : #actions }
Stopwatch >> activate [

	self isSuspended ifTrue:
		[self timespans add: 
			(Timespan starting: DateAndTime now duration: Duration zero).
		self state: #active]

]

{ #category : #accessing }
Stopwatch >> duration [

	| ts last |
	self isSuspended 
		ifTrue:
			[ (ts := self timespans) isEmpty ifTrue: 
				[ ts := { Timespan starting: DateAndTime now duration: Duration zero } ] ]
		ifFalse:
			[ last := self timespans last.
			ts := self timespans allButLast
				add: (last duration: (DateAndTime now - last start); yourself);
				yourself ].
		
	^ (ts collect: [ :t | t duration ]) sum

]

{ #category : #accessing }
Stopwatch >> end [

	^ self timespans last next


]

{ #category : #testing }
Stopwatch >> isActive [

	^ self state = #active

]

{ #category : #testing }
Stopwatch >> isSuspended [

	^ self state = #suspended


]

{ #category : #printing }
Stopwatch >> printOn: aStream [

	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: self state;
		nextPut: $:;
		print: self duration;
		nextPut: $).


]

{ #category : #actions }
Stopwatch >> reActivate [

	self 
		suspend;
		activate.

]

{ #category : #initialization }
Stopwatch >> reset [

	self suspend.
	timespans := nil.


]

{ #category : #actions }
Stopwatch >> start [

	^ self timespans first start


]

{ #category : #accessing }
Stopwatch >> state [

	^ state ifNil: [ state := #suspended ]

]

{ #category : #private }
Stopwatch >> state: aSymbol [

	state := aSymbol

]

{ #category : #actions }
Stopwatch >> suspend [

	| ts |
	self isActive ifTrue:
		[ ts := self timespans last.
		ts duration: (DateAndTime now - ts start).
		self state: #suspended]

]

{ #category : #accessing }
Stopwatch >> timespans [

	^ timespans ifNil: [ timespans := OrderedCollection new ]

]
