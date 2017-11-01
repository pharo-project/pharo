"
I test the serialization of classes and traits as *external* objects, i.e. the classes or traits have to be present in the image at materialization time.
"
Trait {
	#name : #FLTGlobalClassOrTraitSerializationTest,
	#category : 'FuelTests'
}

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testClassSideMethodNotFoundAfterRemoval [
	"Tests that serializer does not tolarate when the method was removed between serialization and materialization"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	classOrTrait classSide duringTestCompileSilently: 'you'.
	
	self serialize: classOrTrait classSide >> #you.
	classOrTrait classSide removeSelectorSilently: #you.
	
	self 
		should: [self materialized] 
		raise: FLMethodNotFound
		whoseDescriptionIncludes: classOrTrait classSide printString 
		description: 'Should raise an error when method is not present.' 
	

]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testClassSideMethodPreservesIdentity [
	"Tests that serialization of a method in the class-side of a class or trait preserves identity"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	classOrTrait classSide duringTestCompileSilently: 'you'.
	self assertSerializationIdentityOf: classOrTrait classSide >> #you
]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testClassSideObsolete [
	"Tests that serializer does not serialize as global the class-side of an obsolete class or trait."
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	self removeFromSystem: classOrTrait.

	self 
		should: [self serialize: classOrTrait ] 
		raise: FLObsolete
		whoseDescriptionIncludes: classOrTrait name
		description: 'Should raise an error when serializing as global an obsolete class or trait.' 
]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testClassSidePreservesIdentity [
	"Tests that serialization of the class side preserves identity"
	
	self assertSerializationIdentityOf: self newClassOrTrait classSide 
]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testMethodNotFoundAfterRemoval [
	"Tests that serializer does not tolarate when the method was removed between serialization and materialization"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	classOrTrait duringTestCompileSilently: 'you'.
	
	self serialize: classOrTrait >> #you.
	classOrTrait removeSelectorSilently: #you.
	
	self 
		should: [self materialized] 
		raise: FLMethodNotFound
		whoseDescriptionIncludes: classOrTrait name, '>>#you' 
		description: 'Should raise an error when method is not present.' 
	

]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testMethodPreservesIdentity [
	"Tests that serialization of a method in a class or trait preserves identity"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	classOrTrait duringTestCompileSilently: 'you'.
	self assertSerializationIdentityOf: classOrTrait >> #you
]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testNotFoundAfterRemoval [
	"Tests that serializer does not tolarate when the class was removed between serialization and materialization"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	
	self serialize: classOrTrait.
	self removeFromSystem: classOrTrait.
	
	self should: [self materialized] raise: FLClassNotFound.
]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testNotFoundAfterRename [
	"Tests that serializer does not tolarate when the class was renamed between serialization and materialization"
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	
	self serialize: classOrTrait.
	classOrTrait renameSilently: (classOrTrait name, 'Renamed') asSymbol.
	
	self should: [self materialized] raise: FLClassNotFound.
]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testObsolete [
	"Tests that serializer does not serialize as global an obsolete class or trait."
	
	| classOrTrait |
	classOrTrait := self newClassOrTrait.
	self removeFromSystem: classOrTrait.

	self 
		should: [self serialize: classOrTrait ] 
		raise: FLObsolete
		whoseDescriptionIncludes: classOrTrait name
		description: 'Should raise an error when serializing as global an obsolete class or trait.' 
]

{ #category : #tests }
FLTGlobalClassOrTraitSerializationTest >> testPreservesIdentity [
	"Tests that serialization of the class or trait preserves identity"
	
	self assertSerializationIdentityOf: self newClassOrTrait
]
