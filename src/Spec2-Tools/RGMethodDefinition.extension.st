Extension { #name : #RGMethodDefinition }

{ #category : #'*Spec2-Tools' }
RGMethodDefinition >> displayName [
	^ String
		streamContents: [ :aStream | 
			self parentName
				ifNotNil: [ aStream
						nextPutAll: self parentName;
						nextPutAll: '>>' ].
			aStream print: self selector ]
]
