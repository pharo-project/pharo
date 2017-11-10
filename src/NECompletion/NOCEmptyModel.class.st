"
I implement a null object pattern related to NOCModel in order to mark the end of possible completions in the answer list. 


"
Class {
	#name : #NOCEmptyModel,
	#superclass : #Object,
	#category : #NECompletion-OCompletion
}

{ #category : #accessing }
NOCEmptyModel >> entries [
	^ Array new
]

{ #category : #accessing }
NOCEmptyModel >> entryCount [ 
	^ 0
]

{ #category : #accessing }
NOCEmptyModel >> narrowString: string [
]

{ #category : #accessing }
NOCEmptyModel >> narrowWith: string [ 

]
