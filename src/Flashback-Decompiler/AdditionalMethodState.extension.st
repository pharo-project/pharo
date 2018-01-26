Extension { #name : #AdditionalMethodState }

{ #category : #'*Flashback-Decompiler' }
AdditionalMethodState >> decompiledArgumentNameAt: aNumber [
	^ 'arg' , aNumber printString
]
