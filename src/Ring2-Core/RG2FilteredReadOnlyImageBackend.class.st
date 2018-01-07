"
Unfinished experiment
"
Class {
	#name : #RG2FilteredReadOnlyImageBackend,
	#superclass : #RG2ReadOnlyImageBackend,
	#category : #'Ring2-Core-Environment'
}

{ #category : #'as yet unclassified' }
RG2FilteredReadOnlyImageBackend >> allowedClasses [ 

	^ { Color. Color class. Object. Object class. ProtoObject. ProtoObject class. Class. Class class. ClassDescription. ClassDescription class. Behavior. Behavior class. Metaclass. Metaclass class. } 
]

{ #category : #environment }
RG2FilteredReadOnlyImageBackend >> behaviorsFor: aRG2Environment do: aBlock [

	self allowedClasses do: [:each |
		| def | 
		def := self definitionFor: each ifAbsentRegister: [each asRing2MinimalDefinitionIn: aRG2Environment].
		aBlock value: def.].
]

{ #category : #environment }
RG2FilteredReadOnlyImageBackend >> definedBehaviorsFor: aRG2PackageDefinition do: aBlock [

	| realPackage |
	
	realPackage := self realPackageFor: aRG2PackageDefinition.
	realPackage ifNotNil: [ 
		realPackage definedClassesDo: [:behaviorName |
			| def cls | 
			cls := Smalltalk classOrTraitNamed: behaviorName.
			(self allowedClasses includes: cls) ifTrue: [ 
				def := self definitionFor: cls ifAbsentRegister: [cls asRing2MinimalDefinitionIn: aRG2PackageDefinition environment].
				aBlock value: def.]]].
]

{ #category : #environment }
RG2FilteredReadOnlyImageBackend >> packagesFor: aRG2Environment do: aBlock [

	(self allowedClasses collect: #package as: Set) do: [:each |
		| def | 
		def := self definitionFor: each ifAbsentRegister: [each asRing2MinimalDefinitionIn: aRG2Environment].
		aBlock value: def.].
]
