Class {
	#name : #MetacelloPackageSpecResolutionError,
	#superclass : #Error,
	#instVars : [
		'packageSpec',
		'repositories',
		'repositoryError'
	],
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #accessing }
MetacelloPackageSpecResolutionError >> description [
    "Return a textual description of the exception."

    ^ 'Could not resolve: ' , packageSpec name , ' [' , packageSpec file , ']' , ' in' , self repositoryString
        ,
            (repositoryError == nil
                ifTrue: [ '' ]
                ifFalse: [ 
                    "report repository error to user here, since failure here is likely to be due to earlier repository error"
                    ' ERROR: ' , repositoryError description printString ])
]

{ #category : #accessing }
MetacelloPackageSpecResolutionError >> packageSpec [
	^ packageSpec
]

{ #category : #accessing }
MetacelloPackageSpecResolutionError >> packageSpec: anObject [
	packageSpec := anObject
]

{ #category : #accessing }
MetacelloPackageSpecResolutionError >> repositories [
	^ repositories
]

{ #category : #accessing }
MetacelloPackageSpecResolutionError >> repositories: anObject [
	repositories := anObject
]

{ #category : #accessing }
MetacelloPackageSpecResolutionError >> repositoryError [
	^ repositoryError
]

{ #category : #accessing }
MetacelloPackageSpecResolutionError >> repositoryError: anObject [
	repositoryError := anObject
]

{ #category : #private }
MetacelloPackageSpecResolutionError >> repositoryString [
    | repositoryString |
    repositoryString := ''.
    self repositories do: [ :repo | repositoryString := repositoryString , ' ' , repo description ].
    ^ repositoryString
]

{ #category : #Signaling }
MetacelloPackageSpecResolutionError >> signal [
  "GemStone needs this to print exception correctly"

  self messageText: self description.
  ^ super signal
]
