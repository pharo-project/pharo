Extension { #name : #RG2MethodDefinition }

{ #category : #'*Ring2-Monticello' }
RG2MethodDefinition >> asMCDefinition [

	^ MCMethodDefinition 
		className: self parent theNonMetaClass name
		classIsMeta: self parent isMeta
		selector: self name
		category: self protocol
		timeStamp: ''
		source: self sourceCode
]

{ #category : #'*Ring2-Monticello' }
RG2MethodDefinition >> asMCMethodDefinition [

	^ self asMCDefinition
]
