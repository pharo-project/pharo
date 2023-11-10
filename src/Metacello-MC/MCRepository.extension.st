Extension { #name : 'MCRepository' }

{ #category : '*Metacello-MC' }
MCRepository >> asRepositorySpecFor: aMetacelloMCProject [

	self subclassResponsibility
]

{ #category : '*Metacello-MC' }
MCRepository >> metacelloProjectClassFor: aScriptEngine [
    ^ MetacelloProject
]

{ #category : '*Metacello-MC' }
MCRepository >> projectVersion: aString [
    "noop"
]
