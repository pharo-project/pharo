Extension { #name : #MethodRemoved }

{ #category : #'*Calypso-SystemQueries' }
MethodRemoved >> affectsMethodsTaggedWith: tagName [
	^(super affectsMethodsTaggedWith: tagName) or: [ protocol = tagName ]
]
