"
I represent an abstract class for rules that are meant exclusively for beginners.

My subclasses will only be enabled if the 'Beginners' profile is selected in the settings. To make rules that are meant for beginners and non-beginners you should inherit from other Rule class and overwrite the 'isBeginnerRule' to return true. 
"
Class {
	#name : #ReExclusivelyBeginnersRule,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #manifest }
ReExclusivelyBeginnersRule class >> isBeginnerRule [
	^ true
]

{ #category : #manifest }
ReExclusivelyBeginnersRule class >> isDefaultRule [
	^ false
]
