Extension { #name : 'RGClassStrategy' }

{ #category : '*Ring-Monticello' }
RGClassStrategy >> asMCDefinition [
	"Here we should remove the use of the category and also add the package tag to the class definition"

	^ (MCClassDefinition named: self owner name)
		  superclassName: (self owner superclass ifNotNil: [ :aSuperclass | aSuperclass name ]);
		  traitComposition: self owner traitCompositionString;
		  classTraitComposition: self owner metaclass traitCompositionString;
		  packageName: self owner category;
		  instVarNames: self instVarNames;
		  classVarNames: self owner classVarNames;
		  poolDictionaryNames: self owner sharedPoolNames;
		  classInstVarNames: self owner metaclass instVarNames;
		  type: self mcType;
		  comment: self owner comment content;
		  commentStamp: self owner comment stamp;
		  yourself
]

{ #category : '*Ring-Monticello' }
RGClassStrategy >> classLayout [
	"TODO"
	^ layout"
		ifNil: [
			| superLayout scope |
			superLayout := superclass classLayout.
			scope := superLayout slotScope extend.
			layout := superLayout class extending: superLayout scope: scope host: self ]"
]

{ #category : '*Ring-Monticello' }
RGClassStrategy >> classPool [

	"TODO"
	^ Dictionary newFrom: (self classVarNames collect: [ :each | each -> nil ])
]

{ #category : '*Ring-Monticello' }
RGClassStrategy >> mcType [

	^ self layout mcType
]

{ #category : '*Ring-Monticello' }
RGClassStrategy >> referencedPackages [

	^ Array with: self package
]
