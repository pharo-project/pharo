Class {
	#name : #MetacelloVersionDefinitionError,
	#superclass : #MetacelloVersionDoesNotExistError,
	#instVars : [
		'exception'
	],
	#category : #Metacello-Core-Exceptions-Errors
}

{ #category : #printing }
MetacelloVersionDefinitionError >> description [
    "Return a textual description of the exception."

    ^ 'The version ' , self versionString printString , ' is not defined in ' , self project label
        , ' for the current platform, because an exception occurred while creating the version:. '
        , self exception description , '. Evaluate the following to see the error: ''['
        , self project configuration class name asString
        ,
            ' project ] 
	on: MetacelloErrorInProjectConstructionNotification 
	do: [:ex | ex resume: true ].'''
        ,
            '
Possible versions include: ' , self possibleVersions printString
]

{ #category : #accessing }
MetacelloVersionDefinitionError >> exception [
	^ exception
]

{ #category : #accessing }
MetacelloVersionDefinitionError >> exception: anObject [
	exception := anObject
]
