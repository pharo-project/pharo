"
Checks for methods whose source sends a different super message.

A common example of this is in creation methods. You might define a method such as:

	createInstance
		^super new initialize

If the new method is not defined in the class, you should probably rewrite this to use self instead. Also, if the new method is defined, you might question why you need to send new to the superclass instead of to the class.
"
Class {
	#name : #ReSendsDifferentSuperRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReSendsDifferentSuperRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReSendsDifferentSuperRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'SendsDifferentSuperRule'
]

{ #category : #running }
ReSendsDifferentSuperRule >> basicCheck: aMethod [
	^ aMethod superMessages anySatisfy: [ :message | message ~= aMethod selector ]
]

{ #category : #accessing }
ReSendsDifferentSuperRule >> group [
	^ 'Design Flaws'
]

{ #category : #accessing }
ReSendsDifferentSuperRule >> name [
	^ 'Sends different super message'
]

{ #category : #accessing }
ReSendsDifferentSuperRule >> rationale [
	^ 'Checks for methods whose source sends a different super message.'
]
