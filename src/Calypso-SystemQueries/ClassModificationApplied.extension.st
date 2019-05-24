Extension { #name : #ClassModificationApplied }

{ #category : #'*Calypso-SystemQueries' }
ClassModificationApplied >> affectsMethod: aMethod [
	"Class modification leads to methods recompilation (because variables are changed).
	So it means that this event affects any method of modified class"
	
	^self classAffected == aMethod origin
]

{ #category : #'*Calypso-SystemQueries' }
ClassModificationApplied >> affectsVariablesOf: aClass [
	"Any class has ClassVariables which are visible from both sides: instance side and class 	side.
	That's any class modification can affect variables from both class sides.
	And we do not need to distinguish it"

	aClass instanceSide = modifiedClass instanceSide ifTrue: [ ^true ].
	
	^false
]
