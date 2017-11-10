"
A critic which represents that the method is missing some required messages after the one in questiong
"
Class {
	#name : #ReLaterInvocationCritique,
	#superclass : #ReInvocationOrderCritique,
	#category : #Renraku-Critiques
}

{ #category : #accessing }
ReLaterInvocationCritique >> description [

	^ 'The functionality of #', targetMethodName, ' is intended to influence ', condition afterDescriptionString, '. In your case it is not followed by the previously mentioned messages which suggests that you are doing something wrong.' 
]

{ #category : #accessing }
ReLaterInvocationCritique >> title [

	^ 'Message #', targetMethodName, ' not followed by ',
		(condition afterDescriptionString truncateWithElipsisTo: 30)
]
