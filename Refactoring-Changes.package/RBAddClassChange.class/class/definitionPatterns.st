private
definitionPatterns
	^ #('`@superclass subclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass subclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableByteSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableByteSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableWordSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass variableWordSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass weakSubclass: `#className instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category'
		'`@superclass weakSubclass: `#className uses: `@traitComposition instanceVariableNames: `#instanceVariableNames classVariableNames: `#classVariableNames poolDictionaries: `#poolDictionaries category: `#category')
		flatCollect: [ :each | Array with: each with: (each copyReplaceAll: '`@superclass' with: 'ProtoObject') , '. `className superclass: `@superclass' ]