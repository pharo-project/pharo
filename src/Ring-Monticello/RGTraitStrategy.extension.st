Extension { #name : 'RGTraitStrategy' }

{ #category : '*Ring-Monticello' }
RGTraitStrategy >> asMCDefinition [

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
