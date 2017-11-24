"
Open an inspector on the remote repository.
"
Class {
	#name : #VSOpenRepositoryCommand,
	#superclass : #VSCommand,
	#category : #'Versionner-Core-Commands'
}

{ #category : #execute }
VSOpenRepositoryCommand >> execute [
	self repository 
		ifNil: [ UIManager default inform: 'No repository associated to this configuration' ]
		ifNotNil: [ :repository | repository morphicOpen ]
]

{ #category : #accessing }
VSOpenRepositoryCommand >> repository [
	^ (target repositories) 
		ifNil: [ nil ]
		ifNotNil: [ :repos |self chooseRepositoryFromList: repos ]

]

{ #category : #accessing }
VSOpenRepositoryCommand >> title [
	^ 'open repository'
]
