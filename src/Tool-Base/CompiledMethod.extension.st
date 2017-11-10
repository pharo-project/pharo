Extension { #name : #CompiledMethod }

{ #category : #'*Tool-Base' }
CompiledMethod >> browse [

	^ Smalltalk tools browser openOnClass: self methodClass selector: self selector
]

{ #category : #'*Tool-Base' }
CompiledMethod >> implementors [
	
	^ SystemNavigation default allImplementorsOf: self selector
]

{ #category : #'*Tool-Base' }
CompiledMethod >> senders [
	
	^ SystemNavigation default allSendersOf: self selector
]
