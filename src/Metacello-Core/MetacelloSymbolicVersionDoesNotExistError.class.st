Class {
	#name : #MetacelloSymbolicVersionDoesNotExistError,
	#superclass : #MetacelloVersionDoesNotExistError,
	#category : #'Metacello-Core-Exceptions-Errors'
}

{ #category : #printing }
MetacelloSymbolicVersionDoesNotExistError >> description [
    "Return a textual description of the exception."

    ^ 'The symbolic version ' , self versionString printString , ' is not defined in ' , self project label
        , ' for the current platform. Possible symbolic version values include: ' , self possibleVersions printString
]

{ #category : #accessing }
MetacelloSymbolicVersionDoesNotExistError >> possibleVersions [
    ^ self project symbolicVersionSymbols
]
