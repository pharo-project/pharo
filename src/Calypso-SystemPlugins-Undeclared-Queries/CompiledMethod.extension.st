Extension { #name : #CompiledMethod }

{ #category : #'*Calypso-SystemPlugins-Undeclared-Queries' }
CompiledMethod >> usesUndeclareds [
	self literalsDo: [:each | 
				each class == UndeclaredVariable ifTrue: [^true].
				(each isBlock not and: [ 
					each value isBehavior and: [ each value isObsolete ]]) ifTrue: [^true]].
	^false
]
