running
basicCheck: aClass 
	
	"The comparison between methods is made using the ast, this is better than comparing source code only since it does not take into account identations, extra parenthesis, etc"

	^ aClass isTrait not and: [ aClass hasTraitComposition and: [  
		aClass localMethods anySatisfy: [ :method | |traitCompositionMethod |
			traitCompositionMethod := (aClass traitComposition methodDescriptionForSelector: method selector) effectiveMethod.
			traitCompositionMethod notNil and: [
				(traitCompositionMethod ast = method ast) ] ] ] ]