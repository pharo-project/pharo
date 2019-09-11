Extension { #name : #RGVariable }

{ #category : #'*Calypso-Ring' }
RGVariable >> isReadIn: aCompiledCode [
"TODO"
	^aCompiledCode readsSlot: self
]

{ #category : #'*Calypso-Ring' }
RGVariable >> isWrittenIn: aCompiledCode [

"TODO"
	^aCompiledCode writesSlot: self
]
