Extension { #name : #MCRepository }

{ #category : #'*metacello-mc' }
MCRepository >> asRepositorySpecFor: aMetacelloMCProject [

	self subclassResponsibility
]

{ #category : #'*metacello-mc' }
MCRepository >> metacelloProjectClassFor: aScriptEngine [
    ^ MetacelloMCProject
]

{ #category : #'*metacello-mc' }
MCRepository >> projectVersion: aString [
    "noop"
]
