Extension { #name : #MethodModified }

{ #category : #'*Calypso-SystemQueries' }
MethodModified >> canAffectResultOfMethodQuery: aMethodQuery [

	^(aMethodQuery isAffectedByChangedMethod: self oldMethod) 
		or: [ aMethodQuery isAffectedByChangedMethod: self newMethod]
]
