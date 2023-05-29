"
I am DomainError, an ArithmeticException indicating that some argument falls outside an expected domain, [from, to]

When my valid interval is left- or right-open, use signal: creation protocol to provide a custom messageText rather than the default [from, to] notation.
"
Class {
	#name : #DomainError,
	#superclass : #ArithmeticError,
	#instVars : [
		'from',
		'to'
	],
	#category : #'Kernel-Exceptions'
}

{ #category : #signaling }
DomainError class >> signal: signallerText from: start [
	^ self signal: signallerText from: start to: Float infinity
]

{ #category : #signaling }
DomainError class >> signal: signallerText from: start to: end [
	^ self new
		from: start;
		to: end;
		signal: signallerText
]

{ #category : #signaling }
DomainError class >> signal: signallerText to: end [
	^ self signal: signallerText from: Float infinity negated to: end
]

{ #category : #signaling }
DomainError class >> signalFrom: start [
	^ self signalFrom: start to: Float infinity
]

{ #category : #signaling }
DomainError class >> signalFrom: start to: end [
	| msgStart msgEnd |
	msgStart := (start isFloat and: [start isFinite not]) ifTrue: ['(-infinity'] ifFalse: ['[', start printString].
	msgEnd := (end isFloat and: [end isFinite not]) ifTrue: ['infinity)'] ifFalse: [end printString, ']'].
	^ self signal: 'Value outside ', msgStart, ' , ' , msgEnd
		from: start
		to: end
]

{ #category : #signaling }
DomainError class >> signalTo: end [
	^ self signalFrom: Float infinity negated to: end
]

{ #category : #accessing }
DomainError >> from [
	^ from
]

{ #category : #accessing }
DomainError >> from: start [
	from := start
]

{ #category : #accessing }
DomainError >> to [
	^ to
]

{ #category : #accessing }
DomainError >> to: end [
	to := end
]
