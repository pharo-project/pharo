Extension { #name : #RGClassDefinitionTest }

{ #category : #'*Manifest-Tests' }
RGClassDefinitionTest >> testArrayStringForManifest [
	| class |
	class := self class.

 	self
		assert: class asRingDefinition arrayStringForManifest
		equals: { #RGClassDefinition . { class name asSymbol } }
]

{ #category : #'*Manifest-Tests' }
RGClassDefinitionTest >> testManifestReadOn [
	| class |
	class := self class.

 	self
		assert: (RGClassDefinition manifestReadOn: { class name asSymbol })
		equals: class asRingDefinition 
]
