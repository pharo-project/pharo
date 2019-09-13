Extension { #name : #RGTraitV2Strategy }

{ #category : #'*Ring-Monticello' }
RGTraitV2Strategy >> asMCDefinition [

	^ MCClassDefinition 
		name: self name
		superclassName: (self superclass ifNil: [ 'nil' ] ifNotNil: [ :aSuperclass | aSuperclass name ])
		traitComposition: self owner traitCompositionString 
		classTraitComposition: self owner metaclass traitCompositionString
		category: self category 
		instVarNames: self instVarNames
		classVarNames: self classVarNames
		poolDictionaryNames: self sharedPoolNames
		classInstVarNames: self metaclass instVarNames
		type: self mcType
		comment: self comment content
		commentStamp: self comment stamp
]

{ #category : #'*Ring-Monticello' }
RGTraitV2Strategy >> classLayout [
	"TODO"
	^ layout"
		ifNil: [ 
			| superLayout scope |
			superLayout := superclass classLayout.
			scope := superLayout slotScope extend.
			layout := superLayout class extending: superLayout scope: scope host: self ]"
]

{ #category : #'*Ring-Monticello' }
RGTraitV2Strategy >> classPool [

	"TODO"
	^ Dictionary newFrom: (self classVarNames collect: [ :each | each -> nil ])
]

{ #category : #'*Ring-Monticello' }
RGTraitV2Strategy >> mcType [

	^ self layout mcType
]

{ #category : #'*Ring-Monticello' }
RGTraitV2Strategy >> referencedPackages [

	^ Array with: self package.
]
