"
I am an action that appears in the Nautiluas qa plugin next the the item's title.

icon - a Form that will appear on the button (green square by default)

description - the description that will be present on popup on hower

action - a two (ortional) parameter block that is evaluated with the critic and the current code entity (class, method…) when the button is pressed
 No newline at end of file

"
Class {
	#name : #RePropertyAction,
	#superclass : #Object,
	#instVars : [
		'action',
		'icon',
		'description'
	],
	#category : #Renraku-Critiques
}

{ #category : #evaluating }
RePropertyAction >> actOnCritic: aCritic ofEntity: anEntity [

	action cull: aCritic cull: anEntity 

]

{ #category : #accessing }
RePropertyAction >> action [
	^ action

]

{ #category : #accessing }
RePropertyAction >> action: anObject [
	action := anObject

]

{ #category : #accessing }
RePropertyAction >> description [
	^ description

]

{ #category : #accessing }
RePropertyAction >> description: anObject [
	description := anObject

]

{ #category : #accessing }
RePropertyAction >> icon [ 
	^ icon

]

{ #category : #accessing }
RePropertyAction >> icon: anObject [
	icon := anObject

]

{ #category : #initialization }
RePropertyAction >> initialize [ 
	
	icon := (Form extent: 16@16 depth: 32) fillColor: Color green.
	description := ''.
	action := [ :critic :entity | ]

]
