Extension { #name : #RG2MethodDefinition }

{ #category : #'*Reflectivity' }
RG2MethodDefinition >> createTwin [
	self compiledMethod createTwin
]

{ #category : #'*Reflectivity' }
RG2MethodDefinition >> installLink: aMetaLink [ 
	self compiledMethod installLink: aMetaLink 
]

{ #category : #'*Reflectivity' }
RG2MethodDefinition >> invalidate [
	self compiledMethod invalidate
]

{ #category : #'*Reflectivity' }
RG2MethodDefinition >> removeLink: aLink [
	self compiledMethod removeLink: aLink
]
