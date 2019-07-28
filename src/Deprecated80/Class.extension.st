Extension { #name : #Class }

{ #category : #'*Deprecated80' }
Class >> definitionForNautilus [
	"Answer a String that defines the receiver."

	self 
		deprecated: 'Use #definition instead.' 
		on: '2019-05-14'
		in: #Pharo8
		transformWith: '`@receiver definitionForNautilus' 
						-> '`@receiver definition'.
						
	^ self definition
]

{ #category : #'*Deprecated80' }
Class >> subclass: aSubclassSymbol instanceVariableNames: instVarNames classVariableNames: classVarNames category: aCategorySymbol [
	"Added to allow for a simplified subclass creation experience. "

	self
		deprecated: 'Use Class>>#subclass:instanceVariableNames:classVariableNames:package: instead'
		transformWith:
			'`@receiver subclass: `@statements1 instanceVariableNames: `@statements2 classVariableNames: `@statements3 category: `@statements4'
				-> '`@receiver subclass: `@statements1 instanceVariableNames: `@statements2 classVariableNames: `@statements3 package: `@statements4'.
	^ self
		subclass: aSubclassSymbol
		instanceVariableNames: instVarNames
		classVariableNames: classVarNames
		package: aCategorySymbol
]

{ #category : #'*Deprecated80' }
Class >> subclass: t instanceVariableNames: f classVariableNames: d poolDictionaries: s category: cat [
	self
		deprecated: 'Use Class>>#subclass:instanceVariableNames:classVariableNames:package: instead'
		transformWith:
			'`@receiver subclass: `@statements1 instanceVariableNames: `@statements2 classVariableNames: `@statements3 poolDictionaries: `@statements4 category: `@statements5'
				-> '`@receiver subclass: `@statements1 instanceVariableNames: `@statements2 classVariableNames: `@statements3 poolDictionaries: `@statements4 package: `@statements5'.
	^ self
		subclass: t
		instanceVariableNames: f
		classVariableNames: d
		poolDictionaries: s
		package: cat
]

{ #category : #'*Deprecated80' }
Class >> subclass: aSubclassSymbol layout: layoutClass slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [
	self deprecated: 'use ... package: instead'.
	^ self
		subclass: aSubclassSymbol
		layout: layoutClass
		slots: slotDefinition
		classVariables: classVarDefinition
		package: aCategorySymbol
]

{ #category : #'*Deprecated80' }
Class >> subclass: aSubclassSymbol  layout: layoutClass  slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [
	self deprecated: 'use ... package: instead'.
	^ self subclass: aSubclassSymbol  layout: layoutClass  slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: aCategorySymbol

]

{ #category : #'*Deprecated80' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition category: aCategorySymbol [
	self deprecated: 'use ... package: instead'.
	^ self
		subclass: aSubclassSymbol
		slots: slotDefinition
		classVariables: classVarDefinition
		package: aCategorySymbol
]

{ #category : #'*Deprecated80' }
Class >> subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames category: aCategorySymbol [

	self deprecated: 'use ... package: instead'.
	^ self subclass: aSubclassSymbol slots: slotDefinition classVariables: classVarDefinition poolDictionaries: someSharedPoolNames package: aCategorySymbol
]
