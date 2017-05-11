visitor
visitMethodChange: aChange
	"Addition and Modification come here"

	self 
		behaviorNamed: aChange behaviorAffectedName
		ifPresent: [ :behavior | 
			^behavior 
				compiledMethodAt: aChange methodAffectedSelector
				ifPresent: [ :method |
					(method protocol ~= aChange methodAffectedProtocol or: [
					method sourceCode ~= aChange methodAffectedSourceCode ]) 
						ifTrue: [ { EpMethodModification oldMethod: method newMethod: aChange methodAffected } ]
						ifFalse: [ #() ] ]
				ifAbsent: [ 
					{ EpMethodAddition method: aChange methodAffected } ] ].
	
	^ { aChange }
