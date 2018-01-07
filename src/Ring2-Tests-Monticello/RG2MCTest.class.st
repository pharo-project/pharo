Class {
	#name : #RG2MCTest,
	#superclass : #RG2Test,
	#category : #'Ring2-Tests-Monticello'
}

{ #category : #tests }
RG2MCTest >> compareTraitComposition: aTraitComposition with: aRG2TraitCompositionDefinition [
	
	self assert: aTraitComposition transformations size equals: aRG2TraitCompositionDefinition transformations size.
	
	aTraitComposition transformations with: aRG2TraitCompositionDefinition transformations do: [ :real :model |
		self assert: real isTrait equals: model isTrait.
		self assert: (real isKindOf: TraitAlias) equals: model isTraitAlias.
		self assert: (real isKindOf: TraitExclusion) equals: model isTraitExclusion.
		(real isKindOf: TraitExclusion) ifTrue: [ 
			self assert: real subject name equals: model subject name.
			real exclusions with: model exclusions do: [:realExclusion :modelExclusion |
				self assert: realExclusion equals: modelExclusion	]].
		(real isKindOf: TraitAlias) ifTrue: [ 
			self assert: real subject name equals: model subject name.
			real aliases with: model aliases do: [:realAlias :modelAlias |
				self assert: realAlias equals: modelAlias ]].
	  ].
	
]

{ #category : #tests }
RG2MCTest >> layoutClassesMapping [

	^ Dictionary newFrom: { 
		CompiledMethodLayout -> RG2CompiledMethodLayoutDefinition. 
		ByteLayout -> RG2ByteLayoutDefinition. 
		ImmediateLayout -> RG2ImmediateLayoutDefinition. 
		WordLayout -> RG2WordLayoutDefinition. 
		WeakLayout -> RG2WeakLayoutDefinition.
		VariableLayout -> RG2VariableLayoutDefinition.
		EphemeronLayout -> RG2EphemeronLayoutDefinition.
		FixedLayout -> RG2FixedLayoutDefinition.
	}
]

{ #category : #tests }
RG2MCTest >> testClass: aClass [
	
	| mcDef def env|
	
	mcDef := 	aClass asClassDefinition.
		
	def := mcDef asRing2Definition.
	env := def environment.
	
	self assert: def name equals: aClass name.
	aClass superclass ifNotNil: [ "ProtoObject & co."
		self assert: def superclass name equals: aClass superclass name.].
	self assert: def category equals: aClass category.
	self assert: def layout class equals: (self layoutClassesMapping at: aClass classLayout class).
	
	def layout isPointerLayout ifTrue: [ 
		self assert: (def layout slots collect: #name) equals: aClass instVarNames.].
	self assert: (def metaclass layout slots collect: #name) equals: aClass class instVarNames.	
	self assert: (def classVariables collect: #name) equals: aClass classVarNames.	
	self assert: (def sharedPoolNames asArray) equals: aClass sharedPoolNames asArray.	
	
	"Monticello class definition still use only class categories, packages are unresolved"
	self assert: (def hasUnresolved: #package).
	
	self assert: (def hasResolved: #traitComposition).
	
	self compareTraitComposition: aClass traitComposition with: def traitComposition.
	self compareTraitComposition: aClass class traitComposition with: def metaclass traitComposition.
	
]
