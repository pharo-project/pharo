"
Non-exisitng selector send in a method that has itself no senders means that the whole method most likely is dead code.
"
Class {
	#name : #ReDeadMethod,
	#superclass : #ReAbstractRule,
	#category : #'Renraku-Rules'
}

{ #category : #'testing-interest' }
ReDeadMethod class >> checksMethod [

	^ true
]

{ #category : #running }
ReDeadMethod >> basicCheck: aMethod [

	^ (aMethod senders isEmpty) and: [ aMethod messages anySatisfy: [ :each | each implementors isEmpty ] ].
]

{ #category : #running }
ReDeadMethod >> group [
	^ 'Optimization'
]

{ #category : #accessing }
ReDeadMethod >> name [
	^ 'Dead Method'
]

{ #category : #accessing }
ReDeadMethod >> rationale [
	^ 'Remove this method which has no incoming nor outgoing calls'
]

{ #category : #running }
ReDeadMethod >> severity [
	^ #warning
]
