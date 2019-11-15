Class {
	#name : #PBClassDefinition,
	#superclass : #Object,
	#instVars : [
		'classDefinition',
		'bootstrapEnvironment'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #'instance-creation' }
PBClassDefinition class >> on: classDefinition environment: aBootstrapEnvironment [ 
	
	^ self new
		initializeOn: classDefinition environment: aBootstrapEnvironment;
		yourself
]

{ #category : #accessing }
PBClassDefinition >> allSlots [
	
	^ classDefinition allSlots
]

{ #category : #accessing }
PBClassDefinition >> definesClassVariable: aString [ 
	
	^ classDefinition instanceSide classVarNames includes: aString
]

{ #category : #'initialize-release' }
PBClassDefinition >> initializeOn: aClassDefinition environment: aBootstrapEnvironment [

	classDefinition := aClassDefinition.
	bootstrapEnvironment := aBootstrapEnvironment.
]

{ #category : #accessing }
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

{ #category : #accessing }
PBClassDefinition >> name [

	^ classDefinition instanceSide name
]

{ #category : #accessing }
PBClassDefinition >> sharedPools [

	^ classDefinition instanceSide sharedPools collect: [ :pool | 
		PBClassDefinition on: pool environment: bootstrapEnvironment ]
]

{ #category : #accessing }
PBClassDefinition >> superclass [
	
	classDefinition superclass ifNil: [ ^ nil ].
	^ PBClassDefinition on: classDefinition superclass environment: bootstrapEnvironment
]

{ #category : #accessing }
PBClassDefinition >> usesClassVariable: aString [ 
	
	^ classDefinition instanceSide allClassVarNames includes: aString
]
