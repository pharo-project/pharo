Extension { #name : 'Class' }

{ #category : '*Fuel-Core' }
Class >> basicClassPool [
	"Answer nil or the dictionary of class variables."

	^classPool
]

{ #category : '*Fuel-Core' }
Class >> basicEnvironment [

	^ environment
]

{ #category : '*Fuel-Core' }
Class >> basicSharedPools [
	"Answer nil or a Set of the pool dictionaries declared in the receiver."

	^sharedPools
]

{ #category : '*Fuel-Core' }
Class >> basicSubclasses [

	^ subclasses
]

{ #category : '*Fuel-Core' }
Class >> basicSubclasses: anArray [

	subclasses := anArray
]

{ #category : '*Fuel-Core' }
Class >> fuelAccept: aGeneralMapper [

	^aGeneralMapper visitClass: self
]
