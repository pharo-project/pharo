"
I'm a domain object used by Equals tests.

I'm an apple with a color.
"
Class {
	#name : 'EqApple',
	#superclass : 'EqFruit',
	#instVars : [
		'color'
	],
	#category : 'Equals-Tests',
	#package : 'Equals-Tests'
}

{ #category : 'instance creation' }
EqApple class >> color: aColor [
	^self new
		color: aColor;
		yourself
]

{ #category : 'accessing' }
EqApple >> color [
	^ color
]

{ #category : 'accessing' }
EqApple >> color: anObject [
	color := anObject
]
