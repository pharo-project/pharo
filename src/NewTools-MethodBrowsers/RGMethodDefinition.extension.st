Extension { #name : #RGMethodDefinition }

{ #category : #'*NewTools-MethodBrowsers' }
RGMethodDefinition >> displayName [

	^ String streamContents: [ :aStream | 
		self parentName 
			ifNotNil: [ aStream << self parentName << '>>' ].
		aStream print: self selector ]
]
