Extension { #name : #MethodRepackaged }

{ #category : #'*Calypso-SystemQueries' }
MethodRepackaged >> affectsClassExtension [
	^true
]

{ #category : #'*Calypso-SystemQueries' }
MethodRepackaged >> affectsClassesExtendedInPackage: aPackage [
	^(super affectsClassesExtendedInPackage: aPackage)
		or: [ oldPackage = aPackage ]
]

{ #category : #'*Calypso-SystemQueries' }
MethodRepackaged >> affectsMethodsDefinedInPackage: aPackage [
	^(super affectsMethodsDefinedInPackage: aPackage)
		or: [ oldPackage = aPackage ]
]

{ #category : #'*Calypso-SystemQueries' }
MethodRepackaged >> affectsPackage: aPackage [
	^newPackage == aPackage or: [ oldPackage == aPackage ]
]
