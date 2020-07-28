"
This smell arises when an instance variable is not both read and written. If an instance variable is only read, the reads can be replaced by nil, since it could not have been assigned a value. If the variable is only written, then it does not need to store the result since it is never used. This check does not work for the data model classes since they use the #instVarAt:put: messages to set instance variables.
"
Class {
	#name : #ReIvarNeitherReadNorWrittenRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #'testing-interest' }
ReIvarNeitherReadNorWrittenRule class >> checksClass [
	^ true
]

{ #category : #enumerating }
ReIvarNeitherReadNorWrittenRule >> check: aClass forCritiquesDo: aCriticBlock [
	aClass slots
		select: [ :slot | slot isReferenced not ]
		thenDo: [ :slot | aCriticBlock cull: (self critiqueFor: aClass about: slot) ]
]

{ #category : #enumerating }
ReIvarNeitherReadNorWrittenRule >> critiqueFor: aClass about: aSlot [

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
ReIvarNeitherReadNorWrittenRule >> name [

	^ 'Instance variable not read or not written'
]

{ #category : #accessing }
ReIvarNeitherReadNorWrittenRule >> severity [
	^ #information
]
