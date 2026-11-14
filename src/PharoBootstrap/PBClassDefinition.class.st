"
I wrap the Ring definition of a class of the image to bootstrap so that it can be used as the class of the methods analyzed by the compiler during the bootstrap.

I resolve the variables of the methods: the slots, the class variables and the shared pools of the class and of its superclasses, then the globals of the bootstrap environment.
"
Class {
	#name : 'PBClassDefinition',
	#superclass : 'Object',
	#instVars : [
		'classDefinition',
		'bootstrapEnvironment'
	],
	#category : 'PharoBootstrap',
	#package : 'PharoBootstrap'
}

{ #category : 'instance creation' }
PBClassDefinition class >> on: classDefinition environment: aBootstrapEnvironment [ 
	
	^ self new
		initializeOn: classDefinition environment: aBootstrapEnvironment;
		yourself
]

{ #category : 'accessing' }
PBClassDefinition >> allSlots [
	
	^ classDefinition allSlots
]

{ #category : 'accessing' }
PBClassDefinition >> definesClassVariable: aString [ 
	
	^ classDefinition instanceSide classVarNames includes: aString
]

{ #category : 'initialization' }
PBClassDefinition >> initializeOn: aClassDefinition environment: aBootstrapEnvironment [

	classDefinition := aClassDefinition.
	bootstrapEnvironment := aBootstrapEnvironment.
]

{ #category : 'accessing' }
PBClassDefinition >> innerBindingOf: aString [

	(self definesClassVariable: aString asSymbol)
		ifTrue: [ ^ EPClassVariableBinding new
				name: aString asSymbol
					ownerName: self name
					objectSpace: bootstrapEnvironment;
				yourself ].
		
	self sharedPools
		do:
			[ :pool | (pool innerBindingOf: aString) ifNotNil: [ :binding | ^ binding ] ].

	self superclass ifNotNil:[	
		(self superclass innerBindingOf: aString)
			ifNotNil: [ :binding | ^ binding ]].

	"If it is a temp or an instance variable"
	^ nil
]

{ #category : 'lookup' }
PBClassDefinition >> lookupVar: aName declare: aBoolean [

	self allSlots detect: [ :e | e name = aName ] ifFound: [ :e | ^ e ].

	Smalltalk globals reservedVariables at: aName ifPresent: [ :e | ^e ].
	
	(self innerBindingOf: aName) ifNotNil: [ :e | ^ e ].
	
	^ bootstrapEnvironment bindingOf: aName
]

{ #category : 'accessing' }
PBClassDefinition >> name [

	^ classDefinition instanceSide name
]

{ #category : 'accessing' }
PBClassDefinition >> sharedPools [

	^ classDefinition instanceSide sharedPools collect: [ :pool | self class on: pool environment: bootstrapEnvironment ]
]

{ #category : 'accessing' }
PBClassDefinition >> superclass [

	classDefinition superclass ifNil: [ ^ nil ].
	^ self class on: classDefinition superclass environment: bootstrapEnvironment
]
