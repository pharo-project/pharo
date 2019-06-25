Extension { #name : #SystemNavigation }

{ #category : #'*Deprecated80' }
SystemNavigation >> allCallsOn: aSymbol from: aClass [
	self deprecated: 'use #allCallsOn: of class directly'.
	^aClass allCallsOn: aSymbol
]

{ #category : #'*Deprecated80' }
SystemNavigation >> allClassesUsingSharedPool: aString [  
	"Answer all classes that uses the shared pool named aString."

	| pool |
	self deprecated: 'just use #poolUsers directly'.
	pool := self environment classNamed: aString ifAbsent: [ ^#() ].
	^pool poolUsers
]

{ #category : #'*Deprecated80' }
SystemNavigation >> isUsedClass: aClass [
	self deprecated: 'just use #isUsed'.
	^aClass isUsed
]
