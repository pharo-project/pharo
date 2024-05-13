Extension { #name : 'RGTraitV2Strategy' }

{ #category : '*Ring-Monticello' }
RGTraitV2Strategy >> asMCDefinition [

	^ (MCTraitDefinition named: self owner name)
		  traitComposition: self owner traitCompositionString;
		  classTraitComposition: self classSide traitCompositionString;
		  packageName: self package name;
		  tagName: (self category = self package name
				   ifTrue: [ nil ]
				   ifFalse: [ self category withoutPrefix: self package name , '-' ]);
		  instVarNames: self instVarNames;
		  classVarNames: self classVarNames;
		  poolDictionaryNames: self sharedPoolNames;
		  classInstVarNames: self classSide instVarNames;
		  comment: self comment content;
		  commentStamp: self comment stamp;
		  yourself
]

{ #category : '*Ring-Monticello' }
RGTraitV2Strategy >> classLayout [
	"TODO"
	^ layout"
		ifNil: [
			| superLayout scope |
			superLayout := superclass classLayout.
			scope := superLayout slotScope extend.
			layout := superLayout class extending: superLayout scope: scope host: self ]"
]

{ #category : '*Ring-Monticello' }
RGTraitV2Strategy >> classPool [

	"TODO"
	^ Dictionary newFrom: (self classVarNames collect: [ :each | each -> nil ])
]

{ #category : '*Ring-Monticello' }
RGTraitV2Strategy >> mcType [

	^ self layout mcType
]

{ #category : '*Ring-Monticello' }
RGTraitV2Strategy >> referencedPackages [

	^ Array with: self package
]
