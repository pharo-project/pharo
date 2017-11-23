"
The critic tells that a required message is missing prior to the one in question
"
Class {
	#name : #RePriorInvocationCritique,
	#superclass : #ReInvocationOrderCritique,
	#category : #'Renraku-Critiques'
}

{ #category : #accessing }
RePriorInvocationCritique >> description [

	^ 'In order for #', targetMethodName, ' to work you have to call ', condition beforeDescriptionString, ' before' 
]

{ #category : #accessing }
RePriorInvocationCritique >> title [

	^ 'Message #', targetMethodName, ' not preceded by ',
		(condition beforeDescriptionString truncateWithElipsisTo: 30)
]
