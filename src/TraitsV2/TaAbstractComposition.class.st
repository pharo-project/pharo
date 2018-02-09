"
I represent a trait composition.
All the trait compositions are represented by operations. 
My subclasses are the possible operations. 

I know how to resolve the methods and slots included in a trait or traited class.

Also I and my subclasses control how the new methods are compiled.
"
Class {
	#name : #TaAbstractComposition,
	#superclass : #Object,
	#category : #'TraitsV2-Compositions'
}

{ #category : #operations }
TaAbstractComposition >> + anotherTalent [
	^ (TaSequence with: self) + anotherTalent asTraitComposition
]

{ #category : #operations }
TaAbstractComposition >> , anotherTalent [
	^ (TaSequence with: self) , anotherTalent asTraitComposition
]

{ #category : #operations }
TaAbstractComposition >> - anArrayOrASymbol [
	| anArray |
	
	anArray := anArrayOrASymbol isArray
		ifTrue: [ anArrayOrASymbol ]
		ifFalse: [ {anArrayOrASymbol} ].
		
	^ TaRemoveMethod remove: anArray to: self
]

{ #category : #operations }
TaAbstractComposition >> -- anSlotName [
	^ TaRemoveSlot named: anSlotName from: self.
]

{ #category : #comparing }
TaAbstractComposition >> = anotherTrait [
	^ self shouldBeImplemented
]

{ #category : #operations }
TaAbstractComposition >> >> anAssociation [
	^ TaRenameSlot from:anAssociation key to: anAssociation value on: self.
]

{ #category : #operations }
TaAbstractComposition >> @ anArray [
	^ TaAliasMethod alias:anArray to: self.
]

{ #category : #operations }
TaAbstractComposition >> @= anAssociation [
	"This performs a deep alias, rewriting all the sendings of this message"
	^ TaDeepAliasMethod alias:anAssociation to: self.
]

{ #category : #operations }
TaAbstractComposition >> addToSequence: sequence [
	sequence addMember: self.
	^ sequence
]

{ #category : #'transforming selectors' }
TaAbstractComposition >> aliasSelector: selector [
	^ self shouldBeImplemented
]

{ #category : #accessing }
TaAbstractComposition >> allSelectors [
	^ self selectors
]

{ #category : #querying }
TaAbstractComposition >> allTraits [
	^ self subclassResponsibility 
]

{ #category : #converting }
TaAbstractComposition >> asTraitComposition [
	^ self
]

{ #category : #accessing }
TaAbstractComposition >> categoryOfMethod: method withSelector: selector [

	^ method category
]

{ #category : #testing }
TaAbstractComposition >> changesSourceCode: aSelector [
	"Checks if a selector should be compiled because its code has been rewritten."
	self subclassResponsibility.
]

{ #category : #'transforming selectors' }
TaAbstractComposition >> classComposition [
	^ self subclassResponsibility 
]

{ #category : #operations }
TaAbstractComposition >> compile: selector into: aClass [
	"Compile the method associated with selector in the receiver's method dictionary."

	| method newMethod sourceCode trailer |
	method := self compiledMethodAt: selector.
	sourceCode := self sourceCodeAt: selector.
	trailer := method trailer.
	
	(trailer sourceCode = sourceCode) ifFalse: [ trailer sourceCode: sourceCode  ].

	newMethod := aClass compiler
				source: sourceCode;
				class: aClass ;
				failBlock: [^ self];
				compiledMethodTrailer: trailer;
				compile.   "Assume OK after proceed from SyntaxError"
				
	selector == newMethod selector ifFalse: [self error: 'selector changed!'].
		
	aClass basicAddSelector: selector withMethod: newMethod.

	aClass organization protocolOrganizer
		classify: selector
		inProtocolNamed: (self categoryOfMethod: method withSelector: selector).		

]

{ #category : #accessing }
TaAbstractComposition >> compiledMethodAt: aSelector [
	self subclassResponsibility 
]

{ #category : #accessing }
TaAbstractComposition >> compiledMethodAt: aSelector ifAbsent: aValuable [

	^ [ self compiledMethodAt: aSelector ]
		on: NotFound
		do: [ ^ aValuable value ]
]

{ #category : #operations }
TaAbstractComposition >> copyMethod: aSelector into: aClass [ 

	| aCompiledMethod source newMethod |
	
	aCompiledMethod := self compiledMethodAt: aSelector.
	source := aCompiledMethod getSourceReplacingSelectorWith: aSelector.
	newMethod := aCompiledMethod copy.
	newMethod selector: aSelector.
	newMethod methodClass: aClass.
		
	"write source to changes file only if we have it (not available during bootstrap)"	
	
	((source includesSubstring: 'self error: ''No decompiler available''') not and: [((Smalltalk globals includesKey: #SourceFiles) and: [ (Smalltalk globals at: #SourceFiles) notNil])]) ifTrue: [ 
		newMethod putSource: source
			withPreamble: [:f | f cr; nextPut: $!; nextChunkPut: 'Trait method'; cr]].

	aClass basicAddSelector: aSelector withMethod: newMethod.

	aClass organization protocolOrganizer
		classify: aSelector
		inProtocolNamed: aCompiledMethod category asSymbol.		
	
	aClass organization removeEmptyCategories.
	
	^ newMethod
]

{ #category : #copying }
TaAbstractComposition >> copyTraitExpression [
	^ self subclassResponsibility 
]

{ #category : #accessing }
TaAbstractComposition >> hasMethod: aSelector [
	[ self compiledMethodAt: aSelector.
	^ true ]
		on: NotFound
		do: [ ^ false ]
]

{ #category : #querying }
TaAbstractComposition >> includesTrait: aTrait [
	^ self allTraits includes: aTrait
]

{ #category : #operations }
TaAbstractComposition >> initializeObject: anObject [
	| selector |
	selector := self initializeSelectorForMe.

	(anObject class allSelectors includes: selector)
		ifTrue: [ selector value: anObject ]
]

{ #category : #initialization }
TaAbstractComposition >> initializeSelectorForMe [
	^ self shouldBeImplemented
]

{ #category : #operations }
TaAbstractComposition >> installSelector: aSelector into: aClass [
	"It is probable the aliased method does not exists any more in the trait composition. This situation is checked when creating the trait composition."

	[ 	
		(self needsRecompilation: aSelector) 
			ifTrue:[ self compile: aSelector into: aClass	]
			ifFalse:[ self copyMethod: aSelector into: aClass ].
			
	 	aClass >> aSelector propertyAt: #traitSource put: (self traitDefining: aSelector) ]
		on: NotFound
		do: [  ]
]

{ #category : #testing }
TaAbstractComposition >> isEmpty [
	^ false
]

{ #category : #'*TraitsV2' }
TaAbstractComposition >> isLocalAliasSelector: aSymbol [
	"Return true if the selector aSymbol is an alias I define."
	^ false
]

{ #category : #'accessing methods' }
TaAbstractComposition >> methodAt: aSelector ifAbsent: aBlockClosure [ 

	self methods do: [ :e | e selector = aSelector ifTrue: [ ^ e ] ].
	
	^ aBlockClosure value.
]

{ #category : #accessing }
TaAbstractComposition >> name [
	self subclassResponsibility 
]

{ #category : #testing }
TaAbstractComposition >> needsRecompilation: aSelector [
	"Checks if a selector should be compiled because it uses new slots or if its code has been rewritten."
	
	^ self slots isEmpty not or: [ self changesSourceCode: aSelector ]

]

{ #category : #querying }
TaAbstractComposition >> originSelectorOf: aSelector [ 
	
	^ aSelector
]

{ #category : #printing }
TaAbstractComposition >> printOn: aStream [

	aStream nextPutAll: self traitCompositionExpression
]

{ #category : #querying }
TaAbstractComposition >> reverseAlias: aSelector [
	
	self subclassResponsibility
]

{ #category : #accessing }
TaAbstractComposition >> selectors [
	self subclassResponsibility 
]

{ #category : #comparing }
TaAbstractComposition >> semanticallyEquals: another [
	^ self selectors = another selectors
		and: [ self methods = another methods
				and: [ self selectors
						allSatisfy: [ :e | (self sourceCodeAt: e) = (another sourceCodeAt: e) ] ] ]
]

{ #category : #accessing }
TaAbstractComposition >> slots [
	self subclassResponsibility 
]

{ #category : #accessing }
TaAbstractComposition >> sourceCodeAt: selector [
	| method |
	method := self compiledMethodAt: selector.
	^ method selector = selector
		ifTrue: [ method sourceCode ]
		ifFalse: [ method getSourceReplacingSelectorWith: selector ]
]

{ #category : #comparing }
TaAbstractComposition >> syntacticallyEquals: another [
	^ self semanticallyEquals: another
]

{ #category : #printing }
TaAbstractComposition >> traitCompositionExpression [

	^ self subclassResponsibility
]

{ #category : #printing }
TaAbstractComposition >> traitCompositionExpressionWithParens [
	^ '(' , self traitCompositionExpression , ')'
]

{ #category : #querying }
TaAbstractComposition >> traitDefining: selector [
	^ self shouldBeImplemented.
]

{ #category : #querying }
TaAbstractComposition >> traitDefining: aSelector ifNone: aBlockClosure [ 
	
	[ ^ self traitDefining: aSelector ] on: NotFound do: [ ^ aBlockClosure value ]
]

{ #category : #querying }
TaAbstractComposition >> traits [
	^ self subclassResponsibility 
]

{ #category : #operations }
TaAbstractComposition >> without: anotherTalent [
	^ self shouldBeImplemented
]
