"
I am Exception used in the Weak collection tests which has a custom pass action.
Instead of using the Exception's default pass mechanism I will call a customized block.
"
Class {
	#name : #MockExceptionWithPassAction,
	#superclass : #Exception,
	#instVars : [
		'passAction'
	],
	#category : 'Collections-Tests-Weak'
}

{ #category : #handling }
MockExceptionWithPassAction >> pass [
	^ passAction cull: self
]

{ #category : #accessing }
MockExceptionWithPassAction >> passAction: aValuable [
	passAction := aValuable
]
