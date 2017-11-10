Extension { #name : #ProtoObject }

{ #category : #'*Glamour-Helpers' }
ProtoObject >> asGlamorousArray [ 
	 
	^Array with: self
]

{ #category : #'*Glamour-Helpers' }
ProtoObject >> asGlamorousMultiValue [
	^ GLMMultiValue with: self
]

{ #category : #'*Glamour-Helpers' }
ProtoObject >> glamourValue: anObject [ 
	 
	| args | 
	args := anObject asGlamorousArray. 
	^self glamourValueWithArgs: args
]

{ #category : #'*Glamour-Helpers' }
ProtoObject >> glamourValueWithArgs: anArray [ 
	 
	^self
]
