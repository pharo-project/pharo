"
I test the serialization of an entire class or trait, in the cases in which they *are not* present in the image at materialization time.
"
Trait {
	#name : 'FLTCreateClassOrTraitSerializationTest',
	#category : 'Fuel-Core-Tests-FullSerialization',
	#package : 'Fuel-Core-Tests',
	#tag : 'FullSerialization'
}

{ #category : 'helpers' }
FLTCreateClassOrTraitSerializationTest >> fullySerialize: aClass [
	self serializer fullySerializeBehavior: aClass.
	self serialize: aClass
]

{ #category : 'helpers' }
FLTCreateClassOrTraitSerializationTest >> materializeSilently: aBoolean [

	| materialized |
	materialized := self materialized.
	self classFactory registerBehavior: materialized.
	aBoolean ifTrue: [ self classFactory createdSilently add: materialized ].
	^ materialized
]

{ #category : 'class-factory' }
FLTCreateClassOrTraitSerializationTest >> newAnonymousClassOrTrait [
	^ self explicitRequirement
]

{ #category : 'helpers' }
FLTCreateClassOrTraitSerializationTest >> resultOfSerializeRemoveAndMaterialize: aClass [
	| wasSilent |
	self fullySerialize: aClass.
	wasSilent := self classFactory createdSilently includes: aClass.
	self classFactory delete: aClass.
	^ self materializeSilently: wasSilent
]

{ #category : 'helpers' }
FLTCreateClassOrTraitSerializationTest >> resultOfSerializeRemoveAndMaterializeAll: objects [

	| classesOrTraits materialized silent |
	silent := OrderedCollection new.
	classesOrTraits := objects select: [ :anObject | anObject isBehavior or: [ anObject isTrait ] ].
	self serializer fullySerializeAllBehaviors: classesOrTraits.
	self serialize: objects.
	classesOrTraits do: [ :aClassOrTrait |
		(self classFactory createdSilently includes: aClassOrTrait) ifTrue: [ silent add: aClassOrTrait name ].
		self classFactory delete: aClassOrTrait ].
	materialized := self materialized.
	materialized
		select: [ :object | object isBehavior ]
		thenDo: [ :behavior |
			self classFactory registerBehavior: behavior.
			(silent includes: behavior name) ifTrue: [ self classFactory createdSilently add: behavior ] ].
	^ materialized
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testAnonymousBehaviorIsSerializable [
	| classOrTrait |
	classOrTrait := self newAnonymousClassOrTrait.
	self deny: (self environmentOfTest includes: classOrTrait).
	
	self serialize: classOrTrait.
	self
		shouldnt: [ self materialized ]
		raise: FLClassNotFound
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCompiledMethodClassBinding [
	"The class binding of each CM has to point to a correct Association, which should not be added to Smalltalk globals."
	
	| class materializedClassOrTrait className method1 method2 |	
	class := self newClassOrTrait.
	self classFactory
		silentlyCompile:  'fortyTwo ^42' in: class;
		silentlyCompile: 'fortyThree ^43' in: class.
	className := class name.

	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: class.
	method1 := materializedClassOrTrait compiledMethodAt: #fortyTwo.
	method2 := materializedClassOrTrait compiledMethodAt: #fortyThree.

	self assert: className = method1 methodClass name.
	self assert: className = method2 methodClass name.
	self assert: method1 classBinding == method2 classBinding.
	self deny: (self environmentOfTest includesKey: className asSymbol)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateBasic [
	"Tests materialization of a class or trait not defined in the image."

	| aClassOrTrait materializedClassOrTrait environment name package packageTag |
	aClassOrTrait := self newClassOrTrait.
	environment := aClassOrTrait environment.
	package := aClassOrTrait package.
	packageTag := aClassOrTrait packageTag.
	name := aClassOrTrait name.

	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClassOrTrait.

	self deny: aClassOrTrait identicalTo: materializedClassOrTrait.
	self assert: environment identicalTo: materializedClassOrTrait environment.
	self assert: package identicalTo: materializedClassOrTrait package.
	self assert: packageTag name equals: materializedClassOrTrait packageTag name.
	self assert: name equals: materializedClassOrTrait name.

	"It is important to notice that Fuel does not add the materialized class or trait into Smalltalk globals."
	self deny: (self environmentOfTest includesKey: name)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithClassSideMethod [
	"Tests materialization of a class-side method in a class or trait not defined in the image."

	| aClassOrTrait materializedClassOrTrait category |
	category := 'tests-class-side'.
	aClassOrTrait := self newClassOrTrait.
	self classFactory
		silentlyCompile:  'fortyTwo ^42'
		in: aClassOrTrait classSide
		protocol: category.

	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClassOrTrait.

	self assert: (materializedClassOrTrait classSide includesSelector: #fortyTwo).
	self assertCollection: #(fortyTwo) hasSameElements: materializedClassOrTrait classSide localSelectors.
	self assert: category equals: (materializedClassOrTrait classSide protocolNameOfSelector: #fortyTwo).
	self assert: 42 equals: ((self newInstanceFrom: materializedClassOrTrait) class perform: #fortyTwo)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithClassTrait [
	"Tests materialization of a class not defined in the image, with a class trait"
	
	| class materializedClassOrTrait aTrait |	
	aTrait := self classFactory silentlyNewTrait.
	self classFactory
		silentlyCompile: 'fortyTwo ^42'
		in: aTrait classSide.
	class := self classFactory silentlyNewClass.
	class addToComposition: aTrait.

	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: class.

	self assert: 1 equals: materializedClassOrTrait traits size.
	self assert: (materializedClassOrTrait traits includes: aTrait).
	self assert: (aTrait users includes: materializedClassOrTrait).
	self assert: materializedClassOrTrait classSide localSelectors isEmpty.
	self assert: 42 equals: ((self newInstanceFrom: materializedClassOrTrait) class perform: #fortyTwo)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithComment [
	"Tests materialization of the comment of a class or trait not defined in the image."
	
	| aClassOrTrait materializedClassOrTrait |	
	aClassOrTrait := self newClassOrTrait.
	aClassOrTrait comment: 'test comment' stamp: 'test stamp'.
	
	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClassOrTrait.

	self assert: 'test comment' = materializedClassOrTrait comment.
	self assert: 'test stamp' = materializedClassOrTrait commentStamp
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithExternalTrait [
	"Tests materialization of a class not defined in the image, with a trait"
	
	| aClassOrTrait materializedClassOrTrait aTrait |	
	aTrait := self classFactory silentlyNewTrait.
	self classFactory
		silentlyCompile:  'fortyTwo ^42'
		in: aTrait.
	aClassOrTrait := self classFactory silentlyNewClass.
	aClassOrTrait addToComposition: aTrait.

	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClassOrTrait.

	self assert: 1 equals: materializedClassOrTrait traits size.
	self assert: (materializedClassOrTrait traits includes: aTrait).
	self assert: (aTrait users includes: materializedClassOrTrait).
	self assert: materializedClassOrTrait localSelectors isEmpty.
	self assert: 42 equals: ((self newInstanceFrom: materializedClassOrTrait) perform: #fortyTwo)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithInstance [
	"Tests materialization of an internal class or trait together with an object using it."
	
	| aClassOrTrait materializedObjects objectsToSerialize anInstance |
	aClassOrTrait := self newClassOrTrait.
	self classFactory
		silentlyCompile: 'fortyTwo ^42'
		in: aClassOrTrait.
	anInstance := self newInstanceFrom: aClassOrTrait.
	objectsToSerialize := Array 
		with: aClassOrTrait 
		with: anInstance
		with: anInstance class.

	materializedObjects := self resultOfSerializeRemoveAndMaterializeAll: objectsToSerialize.

	self assert: (materializedObjects first includesSelector: #fortyTwo).
	self assert: 42 equals: (materializedObjects second perform: #fortyTwo)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithInternalTrait [
	"Tests materialization of a class not defined in the image, with a trait, both internally serialized"
	
	| aClass aTrait serializedArray materializedArray materializedClassOrTrait materializedTrait |	
	aTrait := self classFactory silentlyNewTrait.
	self classFactory
		silentlyCompile:  'fortyTwo ^42'
		in: aTrait.
	aClass := self newClassOrTrait.
	aClass addToComposition: aTrait.
	serializedArray := Array with: aClass with: aTrait.

	materializedArray := self resultOfSerializeRemoveAndMaterializeAll: serializedArray.
	materializedClassOrTrait := materializedArray first.
	materializedTrait := materializedArray second.

	self assert: 1 equals: (materializedClassOrTrait traits size).
	self assert: (materializedClassOrTrait traits includes: materializedTrait).
	self assert: (materializedTrait users includes: materializedClassOrTrait).
	self assert: materializedClassOrTrait localSelectors isEmpty.
	
	self assert: 42 equals: ((self newInstanceFrom: materializedClassOrTrait) perform: #fortyTwo)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithMethod [
	"Tests materialization of a compiled method in a class not defined in the image."
	
	| aClassOrTrait materializedClassOrTrait category |
	category := 'category-for-fuel-tests'.
	aClassOrTrait := self newClassOrTrait.
	self classFactory
		silentlyCompile: 'fortyTwo ^42'
		in: aClassOrTrait
		protocol: category.

	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClassOrTrait.

	self assert: (materializedClassOrTrait includesSelector: #fortyTwo).
	self assertCollection: #(fortyTwo) hasSameElements: materializedClassOrTrait localSelectors.
	self assert: category equals: (materializedClassOrTrait protocolNameOfSelector: #fortyTwo).
	self assert: 42 equals: ((self newInstanceFrom: materializedClassOrTrait) perform: #fortyTwo).
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testCreateWithTraitOnClassSide [
	"Tests materialization of a class not defined in the image, with a trait added to its class side"
	
	| aClass materializedClassOrTrait aTrait |	
	aTrait := self classFactory silentlyNewTrait.
	self classFactory
		silentlyCompile:  'fortyTwo ^42'
		in: aTrait.
	aClass := self classFactory silentlyNewClass.
	aClass classSide addToComposition: aTrait.

	materializedClassOrTrait := self resultOfSerializeRemoveAndMaterialize: aClass.

	self assert: 1 equals: materializedClassOrTrait classSide traits size.
	self assert: (materializedClassOrTrait classSide traits includes: aTrait).
	self assert: (aTrait users includes: materializedClassOrTrait classSide).
	self assert: materializedClassOrTrait classSide localSelectors isEmpty.
	self assert: 42 equals: ((self newInstanceFrom: materializedClassOrTrait) class perform: #fortyTwo)
]

{ #category : 'tests' }
FLTCreateClassOrTraitSerializationTest >> testDoesNotCreatePackage [
	"Tests materialization of a package not defined in the image."

	| undefinedPackage aClassOrTrait materializedClassOrTrait package packageTag wasSilent |
	undefinedPackage := self packageOrganizer undefinedPackage.
	aClassOrTrait := self newClassOrTrait.
	package := aClassOrTrait package.
	packageTag := aClassOrTrait packageTag.
	
	self deny: package identicalTo: undefinedPackage.

	self fullySerialize: aClassOrTrait.
	wasSilent := self classFactory createdSilently includes: aClassOrTrait.
	self classFactory delete: aClassOrTrait.
	
	package removeFromSystem.
	self deny: (self packageOrganizer hasPackage: package).
	
	materializedClassOrTrait := self materializeSilently: wasSilent.

	self deny: package identicalTo: materializedClassOrTrait package.
	self assert: materializedClassOrTrait package identicalTo: undefinedPackage.
	self deny: packageTag name equals: materializedClassOrTrait packageTag name.
	self assert: materializedClassOrTrait packageTag identicalTo: undefinedPackage undefinedTag
]
