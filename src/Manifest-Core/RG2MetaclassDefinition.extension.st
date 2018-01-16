Extension { #name : #RG2MetaclassDefinition }

{ #category : #'*Manifest-Core' }
RG2MetaclassDefinition >> arrayStringForManifest [
	| array |
	array := Array new: 2.
	array at: 1 put: self class name asSymbol.
	array
		at: 2
		put:
			{(name asSymbol).
			(self theNonMetaClass asString asSymbol)}.
	^ array
]

{ #category : #'*Manifest-Core' }
RG2MetaclassDefinition class >> manifestReadOn: aArray [

	^ (self named: ( aArray at:1))
		instanceSide name: (aArray at:2);
		yourself
]
