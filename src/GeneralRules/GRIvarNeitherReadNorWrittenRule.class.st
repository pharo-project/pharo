"
This smell arises when an instance variable is not both read and written. If an instance variable is only read, the reads can be replaced by nil, since it could not have been assigned a value. If the variable is only written, then it does not need to store the result since it is never used. This check does not work for the data model classes since they use the #instVarAt:put: messages to set instance variables.
"
Class {
	#name : #GRIvarNeitherReadNorWrittenRule,
	#superclass : #ReAbstractRule,
	#category : #GeneralRules
}

{ #category : #'testing-interest' }
GRIvarNeitherReadNorWrittenRule class >> checksClass [
	^ true
]

{ #category : #enumerating }
GRIvarNeitherReadNorWrittenRule >> check: aClass forCritiquesDo: aCriticBlock [
	| methods |
	
	methods := aClass withAllSubclasses flatCollect: #methods.
	
	aClass slots
		select: [ :slot |
			| read written |
			read := written := false.
			methods noneSatisfy: [ :method |
				read := read or: [ method readsSlot: slot ].
				written := written or: [ method writesSlot: slot ].
				read and: [ written ] ] ]
		thenDo: [ :slot |
			aCriticBlock cull:
				(self critiqueFor: aClass about: slot) ]
]

{ #category : #enumerating }
GRIvarNeitherReadNorWrittenRule >> critiqueFor: aClass about: aSlot [

	| crit |
	crit := ReTrivialCritique
		withAnchor: (ReVarSearchSourceAnchor
			entity: aClass
			string: aSlot name)
		by: self.
	
	crit tinyHint: aSlot name.
	^ crit
]

{ #category : #accessing }
GRIvarNeitherReadNorWrittenRule >> name [

	^ 'Instance variable not read or not written'
]

{ #category : #accessing }
GRIvarNeitherReadNorWrittenRule >> severity [
	^ #information
]
