composition
+ aTraitOrTraitComposition
	"Use double dispatch to avoid having nested composition in cases where
	parenthesis are used, such as T1 + (T2 + T3)"
	
	^aTraitOrTraitComposition addOnTheLeft: self