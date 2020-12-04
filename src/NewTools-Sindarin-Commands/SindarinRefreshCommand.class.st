Class {
	#name : #SindarinRefreshCommand,
	#superclass : #SindarinCommand,
	#category : #'NewTools-Sindarin-Commands'
}

{ #category : #default }
SindarinRefreshCommand class >> defaultDescription [
	^ 'Refresh the UI. Necessary after manual control of Sindarin.'
]

{ #category : #initialization }
SindarinRefreshCommand class >> defaultIconName [
	^#refresh 
]

{ #category : #default }
SindarinRefreshCommand class >> defaultName [
	^ 'Refresh'
]

{ #category : #hooks }
SindarinRefreshCommand >> execute [
	self context refresh
]
