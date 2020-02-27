Class {
	#name : #RGMCTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Monticello'
}

{ #category : #tests }
RGMCTest >> compareTraitComposition: aTraitComposition with: anRGTraitComposition [
	
	self assert: aTraitComposition transformations size equals: anRGTraitComposition transformations size.
	
	aTraitComposition transformations with: anRGTraitComposition transformations do: [ :real :model |
		self assert: real isTrait equals: model isTrait.
		self assert: real isTraitAlias equals: model isTraitAlias.
		self assert: real isTraitExclusion equals: model isTraitExclusion.
		real isTraitExclusion ifTrue: [ 
			self assert: real subject name equals: model subject name.
			real exclusions with: model exclusions do: [:realExclusion :modelExclusion |
				self assert: realExclusion equals: modelExclusion	]].
		real isTraitAlias ifTrue: [ 
			self assert: real subject name equals: model subject name.
			real aliases associations with: model aliases do: [:realAlias :modelAlias |
				self assert: realAlias equals: modelAlias ]].
	  ].
	
]

{ #category : #tests }
RGMCTest >> layoutClassesMapping [

	^ Dictionary newFrom: { 
		CompiledMethodLayout -> RGCompiledMethodLayout. 
		ByteLayout -> RGByteLayout. 
		ImmediateLayout -> RGImmediateLayout. 
		WordLayout -> RGWordLayout. 
		WeakLayout -> RGWeakLayout.
		VariableLayout -> RGVariableLayout.
		EphemeronLayout -> RGEphemeronLayout.
		FixedLayout -> RGFixedLayout.
	}
]

{ #category : #tests }
RGMCTest >> testClass: aClass [
	
	| mcDef def env|
	
	mcDef := 	aClass asClassDefinition.
		
	def := mcDef asRGDefinition.
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
