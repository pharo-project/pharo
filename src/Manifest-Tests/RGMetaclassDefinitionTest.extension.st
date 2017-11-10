Extension { #name : #RGMetaclassDefinitionTest }

{ #category : #'*Manifest-Tests' }
RGMetaclassDefinitionTest >> testArrayStringForManifest [
	| class |
	class := self class class.

 	self
		assert: class asRingDefinition arrayStringForManifest
		equals: { #RGMetaclassDefinition . { class name asSymbol . self class name asSymbol } }
]

{ #category : #'*Manifest-Tests' }
RGMetaclassDefinitionTest >> testManifestReadOn [
	| class |
	class := self class.

 	self
		assert: (RGMetaclassDefinition manifestReadOn: { class class name asSymbol . class name asSymbol })
		equals: class class asRingDefinition 

]
