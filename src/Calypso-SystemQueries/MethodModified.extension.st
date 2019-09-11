Extension { #name : #MethodModified }

{ #category : #'*Calypso-SystemQueries' }
MethodModified >> affectsMethod: aMethod [
	^(super affectsMethod: aMethod) or: [oldMethod == aMethod]
]

{ #category : #'*Calypso-SystemQueries' }
MethodModified >> affectsMethodsDefinedInPackage: aPackage [

	^(super affectsMethodsDefinedInPackage: aPackage) or: [ 
		oldMethod package == aPackage ]
]

{ #category : #'*Calypso-SystemQueries' }
MethodModified >> affectsMethodsTaggedWith: tagName [
	^(super affectsMethodsTaggedWith: tagName) or: [ oldProtocol = tagName ]
]

{ #category : #'*Calypso-SystemQueries' }
MethodModified >> canAffectResultOfMethodQuery: aMethodQuery [

	^(aMethodQuery isAffectedByChangedMethod: self oldMethod) 
		or: [ aMethodQuery isAffectedByChangedMethod: self newMethod]
]
