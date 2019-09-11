Extension { #name : #RGSlot }

{ #category : #'*Calypso-Ring' }
RGSlot >> isReadIn: aCompiledCode [
	^aCompiledCode readsSlot: self
]

{ #category : #'*Calypso-Ring' }
RGSlot >> isWrittenIn: aCompiledCode [
	^aCompiledCode writesSlot: self
]
