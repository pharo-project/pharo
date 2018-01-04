Extension { #name : #RG2ClassDefinitionTest }

{ #category : #'*Manifest-Tests' }
RG2ClassDefinitionTest >> testArrayStringForManifest [
	| class |
	class := self class.

 	self
		assert: class asRing2Definition arrayStringForManifest
		equals: { #RG2ClassDefinition . { class name asSymbol } }
]

{ #category : #'*Manifest-Tests' }
RG2ClassDefinitionTest >> testArrayStringForManifestForMetaclass [
	| class |
	class := self class class.

 	self
		assert: class asRing2Definition setNameFromSiblingIfNeeded arrayStringForManifest
		equals: { #RG2MetaclassDefinition . { class name asSymbol . self class name asSymbol } }
]

{ #category : #'*Manifest-Tests' }
RG2ClassDefinitionTest >> testAsMetaclassDefinition [
	| rgClass class |
	class := Class class.
	rgClass:= class asRing2Definition.
	
	self assert: (rgClass isRing2Object).
	self assert: (rgClass isMeta).  
	self assert: (rgClass name = 'Class class').  
	self assert: (rgClass theMetaClass == rgClass).

]

{ #category : #'*Manifest-Tests' }
RG2ClassDefinitionTest >> testManifestReadOn [
	| class |
	class := self class.

 	self
		assert: (RG2ClassDefinition manifestReadOn: { class name asSymbol }) name
		equals: class asRingDefinition name
]
