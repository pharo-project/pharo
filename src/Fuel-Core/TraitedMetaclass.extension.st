Extension { #name : 'TraitedMetaclass' }

{ #category : '*Fuel-Core' }
TraitedMetaclass >> fuelAccept: aGeneralMapper [
	^ aGeneralMapper visitTraitedMetaclass: self
]

{ #category : '*Fuel-Core' }
TraitedMetaclass >> fuelSetTraitComposition: aTraitComposition [
	"prevent checks while the composition hasn't been
	fully materialized yet"
	composition := aTraitComposition
]

{ #category : '*Fuel-Core' }
TraitedMetaclass >> localMethodDict: aMethodDictionary [
	localMethods := aMethodDictionary
]
