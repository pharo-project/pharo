Class {
	#name : #SpCodeBrowsingCommand,
	#superclass : #SpCodeSelectionCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #accessing }
SpCodeBrowsingCommand >> browserClientProvider [
	^ SystemNavigation default
]

{ #category : #accessing }
SpCodeBrowsingCommand >> selector [ 
	^self selection asSymbol
]
