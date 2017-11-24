"
I represent a message to be scheduled by the WorldState.

For example, you can see me in action with the following example which print 'alarm test' on Transcript one second after evaluating the code:

Transcript open.
MorphicUIManager currentWorld
        addAlarm: #show: 
        withArguments: #('alarm test') 
        for: Transcript 
        at: (Time millisecondClockValue + 1000).

* Note *
Compared to doing:
[(Delay forMilliseconds: 1000) wait. Transcript show: 'alarm test'] forkAt: Processor activeProcess priority +1.

the alarm system has several distinctions:
- Runs with the step refresh rate resolution.
- Alarms only run for the active world. (Unless a non-standard scheduler is in use)
- Alarms with the same scheduled time are guaranteed to be executed in the order they were added
"
Class {
	#name : #MorphicAlarm,
	#superclass : #MessageSend,
	#instVars : [
		'scheduledTime',
		'numArgs'
	],
	#category : #'Morphic-Base-Events'
}

{ #category : #'instance creation' }
MorphicAlarm class >> scheduledAt: scheduledTime receiver: aTarget selector: aSelector arguments: argArray [
	^(self receiver: aTarget selector: aSelector arguments: argArray)
		scheduledTime: scheduledTime.
]

{ #category : #accessing }
MorphicAlarm >> scheduledTime [
	"Return the time (in milliseconds) that the receiver is scheduled to be executed"
	^scheduledTime
]

{ #category : #accessing }
MorphicAlarm >> scheduledTime: msecs [
	"Set the time (in milliseconds) that the receiver is scheduled to be executed"
	scheduledTime := msecs
]

{ #category : #evaluating }
MorphicAlarm >> value: anArgument [
	| nArgs |
	numArgs ifNil:[numArgs := selector numArgs].
	nArgs := arguments ifNil:[0] ifNotNil:[arguments size].
	nArgs = numArgs ifTrue:[
		"Ignore extra argument"
		^self value].
	^arguments ifNil: [receiver perform: selector with: anArgument]
		ifNotNil: [receiver perform: selector withArguments: (arguments copyWith: anArgument)]
]
