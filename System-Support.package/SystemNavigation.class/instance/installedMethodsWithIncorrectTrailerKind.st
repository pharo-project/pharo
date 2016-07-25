query
installedMethodsWithIncorrectTrailerKind

	^self allMethodsSelect:
		[:each | (#(#SourcePointer #VarLengthSourcePointer #NativeCodeTrailer) includes: each trailer kind) not and: [ each isInstalled ]  ]