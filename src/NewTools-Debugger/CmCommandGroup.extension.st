Extension { #name : #CmCommandGroup }

{ #category : #'*NewTools-Debugger' }
CmCommandGroup >> commandOrGroupNamed: aString ifNone: aBlock [
	^ entries
		detect: [ :commandOrRegister | commandOrRegister name = aString ]
		ifNone: aBlock
]
