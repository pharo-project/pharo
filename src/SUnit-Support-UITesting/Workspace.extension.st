Extension { #name : #Workspace }

{ #category : #'*SUnit-Support-UITesting' }
Workspace >> hasUnacceptedEdits [
	"Answer true if any of the views on this model has unaccepted edits."

	self dependents
		do: [:each | 
			self flag: #pharoFixMe. "The check for #respondsTo: is a hack necessary just because the old
			Browser uses it wrong. We need to change this after old Browser removal"
			((each respondsTo: #hasUnacceptedEdits) and: [ each hasUnacceptedEdits ]) 
				ifTrue: [^ true]]
		without: self.
	^ false

]
