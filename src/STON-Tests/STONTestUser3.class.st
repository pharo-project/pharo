"
STONTestUser3 extends STONTestUser but wants nil instance variables to be written
"
Class {
	#name : #STONTestUser3,
	#superclass : #STONTestUser,
	#category : #'STON-Tests-Support'
}

{ #category : #'ston-core' }
STONTestUser3 class >> stonAllInstVarNames [
	^ #(username password enabled)
]

{ #category : #'ston-core' }
STONTestUser3 class >> stonName [
	^ #TestUser3
]

{ #category : #'ston-core' }
STONTestUser3 >> stonShouldWriteNilInstVars [
	^ true
]
