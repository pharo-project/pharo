"
I model the write to variables. I have all the information needed to do the write when send #value
"
Class {
	#name : #RFVariableWrite,
	#superclass : #RFVariableAccess,
	#instVars : [
		'assignedValue'
	],
	#category : #'Reflectivity-Operations'
}

{ #category : #accessing }
RFVariableWrite >> assignedValue [
	^ assignedValue
]

{ #category : #accessing }
RFVariableWrite >> assignedValue: anObject [
	assignedValue := anObject
]
