Extension { #name : #RGMethodDefinition }

{ #category : #'*Reflectivity' }
RGMethodDefinition >> createTwin [
	self compiledMethod createTwin
]

{ #category : #'*Reflectivity' }
RGMethodDefinition >> installLink: aMetaLink [ 
	self compiledMethod installLink: aMetaLink 
]

{ #category : #'*Reflectivity' }
RGMethodDefinition >> invalidate [
	self compiledMethod invalidate
]

{ #category : #'*Reflectivity' }
RGMethodDefinition >> removeLink: aLink [
	self compiledMethod removeLink: aLink
]

{ #category : #'*Reflectivity' }
RGMethodDefinition >> variableNodes [
	^self compiledMethod variableNodes.
]
