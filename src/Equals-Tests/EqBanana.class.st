"
I'm a domain object used by Equals tests.

I'm an banana.
"
Class {
	#name : 'EqBanana',
	#superclass : 'EqFruit',
	#category : 'Equals-Tests',
	#package : 'Equals-Tests'
}

{ #category : 'accessing' }
EqBanana >> color [
	^Color yellow
]
