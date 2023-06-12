Class {
	#name : #RBAbstractEnvTestCase,
	#superclass : #ParametrizedTestCase,
	#instVars : [
		'testingEnvironment',
		'model'
	],
	#category : #'Refactoring2-Transformations-Tests-Parametrized'
}

{ #category : #utilities }
RBAbstractEnvTestCase class >> makeAllFailureShownInTestMethodNames [
	
	| methods nSel |
	methods := OrderedCollection new.
	RBAbstractEnvTestCase withAllSubclassesDo: [ :cls |

	methods addAll: (((cls methodsInProtocol: 'failure tests') 
		select: [ :each | 'test*' match: each selector  ] )
		reject: [ :each | 'testFailure*' match: each selector ]). ].


	methods do: [ :meth |
		nSel := 'testFailure', (meth selector allButFirst: 4) asSymbol.
	meth methodClass  
	compile: (meth ast selector: nSel) formattedCode 
	classified: 'failure tests']
]

{ #category : #utilities }
RBAbstractEnvTestCase class >> removeDuplicates [
	
	| methods  |
	methods := OrderedCollection new.
	RBAbstractEnvTestCase withAllSubclassesDo: [ :cls |

	methods addAll: (((cls methodsInProtocol: 'failure tests') 
		select: [ :each | 'test*' match: each selector  ] )
		reject: [ :each | 'testFailure*' match: each selector ]). ].
	methods do: [ :each | 
			each methodClass removeSelector:  each selector
			 ]

	
]

{ #category : #'mocking helpers' }
RBAbstractEnvTestCase >> addMethodsToModelClasses: aCollection in: newModel [
	
	aCollection do: [:each |
			| class |
			class := newModel classNamed: each first.
			each last do: [ :methodPair |
				class
					compile: methodPair first
					classified: methodPair last]].
	^ newModel
]

{ #category : #mocking }
RBAbstractEnvTestCase >> childrenToSiblingTestData [

	^ Smalltalk compiler evaluate: '
	| m |
	(m:= RBNamespace
			onEnvironment: ((RBClassEnvironment onEnvironment: RBBrowserEnvironment new)
					classes: (#(#ConcreteSubclass #ConcreteSuperclass #NoMoveSubclass)
							inject: OrderedCollection new
							into:
								[:sum :each |
								| class |
								class := Smalltalk at: each ifAbsent: [nil].
								class notNil ifTrue: [sum add: class].
								sum])
								, (#(#ConcreteSubclass #ConcreteSuperclass #NoMoveSubclass)
										inject: OrderedCollection new
										into:
											[:sum :each |
											| class |
											class := Smalltalk at: each ifAbsent: [nil].
											class notNil ifTrue: [sum add: class class].
											sum]))
						not)
		name: ''Test''.
#(
''ConcreteSuperclass << #NoMoveSubclass
	package: ''''Testing'''''' 

''Object << #ConcreteSuperclass
	slots: {#instVarName1 . #instVarName2}; 
	sharedVariables: { #ClassVarName1 . #ClassVarName2}; 
	package: ''''Testing'''''' 
	
''ConcreteSuperclass << #ConcreteSubclass
	package: ''''Testing'''''')
	do: [:each | m defineClass: each].
(m metaclassNamed: #ConcreteSuperclass)
	addInstanceVariable: ''classInstVarName1''.
#(#(#ConcreteSubclass #(#(''initialize
	super initialize.
	instVarName1 := nil'' #everyone) #(''different
	^0'' #everyone))) 
	
	#(#ConcreteSuperclass #(#(''same
	^self initialize isKindOf: ConcreteSuperclass'' #''one def'') #(''different
	^instVarName1 + instVarName2'' #everyone) #(''initialize
	instVarName1 := instVarName2 := ClassVarName1 := ClassVarName2 := 0'' #everyone))) 
	
	#(#NoMoveSubclass #(#(''same
	^123'' #''one def''))) 
	#(''ConcreteSubclass class'' #(#(''bar
	^self storeString'' #testing))) 
	#(''ConcreteSuperclass class'' #(#(''foo
	^classInstVarName1 + ClassVarName1 + ClassVarName2'' #testing) #(''new
	^super new initialize'' #testing) 
	#(''bar
	^self printString'' #testing))) 
	#(''NoMoveSubclass class'' #()))
	do:
		[:each |
		| class |
		class := m classNamed: each first.
		each last do: [:meth | class compile: meth first classified: meth last]].
m
'
]

{ #category : #accessing }
RBAbstractEnvTestCase >> defaultNamespaceClass [
	^ RBNamespace
]

{ #category : #accessing }
RBAbstractEnvTestCase >> environmentOfTest [
	^ testingEnvironment
]

{ #category : #mocking }
RBAbstractEnvTestCase >> inlineMethodTestData [

	^ Smalltalk compiler evaluate: '
		| m |
		(m := RBNamespace
			onEnvironment: ((RBClassEnvironment onEnvironment: RBBrowserEnvironment new)
					classes: (#(#RBRenameClassVariableChange #RBVariableCompositeRefactoryChange #RBRenameInstanceVariableChange #RBCompositeRefactoryChange #RBRenameVariableChange #RBRefactoryChange)
							inject: OrderedCollection new
							into:
								[:sum :each |
								| class |
								class := Smalltalk at: each ifAbsent: [nil].
								class notNil ifTrue: [sum add: class].
								sum])
								, (#() inject: OrderedCollection new
										into:
											[:sum :each |
											| class |
											class := Smalltalk at: each ifAbsent: [nil].
											class notNil ifTrue: [sum add: class class].
											sum]))
						not)
		name: ''Test''.
#(

''Object << #RBRefactoryChange
	slots: {#name};
	package: ''''Refactory-Support'''''' 
	
''RBRefactoryChange << #RBCompositeRefactoryChange
	slots: { #changes };
	package: ''''Refactory-Support'''''' 

''RBCompositeRefactoryChange << #RBVariableCompositeRefactoryChange
	slots: { #className . #isMeta };
	package: ''''Refactory-Support'''''' 
	
''RBVariableCompositeRefactoryChange << #RBRenameVariableChange
	slots: { #oldName .  #newName};
	package: ''''Refactory-Support'''''' 

''RBRenameVariableChange << #RBRenameClassVariableChange
	package: ''''Refactory-Support'''''' 
	
''RBRenameVariableChange << #RBRenameInstanceVariableChange
	package: ''''Refactory-Support'''''')
	do: [:each | m defineClass: each].
	
#(#(#RBRenameClassVariableChange #(#(''removeOldVariable
	(RBRemoveClassVariableChange remove: oldName from: self changeClass) execute'' #private) #(''addNewVariable
	(RBAddClassVariableChange add: newName to: self changeClass) execute'' #private) #(''executeNotifying: aBlock
	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	self removeOldVariable.
	^undo'' #private) #(''copyOldValuesToNewVariable
	| oldValue |
	oldValue := self changeClass classPool at: oldName ifAbsent: [].
	self changeClass at: newName asSymbol put: oldValue'' #private))) #(#RBVariableCompositeRefactoryChange #(#(''displayClassName
	^isMeta
		ifTrue: [self changeClassName , '''' class'''']
		ifFalse: [self changeClassName asString]'' #printing) #(''isMeta
	^isMeta'' #private) #(''changeClass: aBehavior
	isMeta := aBehavior isMeta.
	className := isMeta
				ifTrue: [aBehavior soleInstance name]
				ifFalse: [aBehavior name]'' #accessing) #(''hash
	^self changeClassName hash'' #comparing) #(''changeClassName: aSymbol
	className := aSymbol.
	isMeta isNil ifTrue: [isMeta := false]'' #accessing) #(''changeClass
	| class |
	class := Smalltalk at: self changeClassName ifAbsent: [^nil].
	^isMeta ifTrue: [class class] ifFalse: [class]'' #accessing) #(''printOn: aStream
	aStream nextPutAll: self displayString'' #printing) #(''changeClassName
	^className'' #accessing) #(''= aRefactoryClassChange
	self class = aRefactoryClassChange class ifFalse: [^false].
	^className = aRefactoryClassChange changeClassName
		and: [isMeta = aRefactoryClassChange isMeta]'' #comparing))) #(#RBRenameInstanceVariableChange #(#(''removeOldVariable
	(RBRemoveInstanceVariableChange remove: oldName from: self changeClass)
		execute'' #private) #(''addNewVariable
	(RBAddInstanceVariableChange add: newName to: self changeClass) execute'' #private) #(''executeNotifying: aBlock
	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	self removeOldVariable.
	^undo'' #private) #(''copyOldValuesToNewVariable
	| newIndex oldIndex |
	oldIndex := self changeClass allInstVarNames indexOf: oldName asString.
	newIndex := self changeClass allInstVarNames indexOf: newName asString.
	self changeClass withAllSubclasses do:
			[:each |
			each allInstances
				do: [:inst | each instVarAt: newIndex put: (each instVarAt: oldIndex)]]'' #private))) #(#RBCompositeRefactoryChange #(#(''compile: source in: class
	^self addChange: (RBAddMethodChange compile: source in: class)'' #''refactory-changes'') #(''addClassVariable: variableName to: aClass
	^self addChange: (RBAddClassVariableChange add: variableName to: aClass)'' #''refactory-changes'') #(''addChangeFirst: aRefactoryChange
	changes addFirst: aRefactoryChange.
	^aRefactoryChange'' #accessing) #(''removeChange: aChange
	changes remove: aChange ifAbsent: []'' #''private-inspector accessing'') #(''addPool: aPoolVariable to: aClass
	^self addChange: (RBAddPoolVariableChange add: aPoolVariable to: aClass)'' #''refactory-changes'') #(''initialize
	super initialize.
	changes := OrderedCollection new'' #''initialize-release'') #(''defineClass: aString
	^self addChange: (RBAddClassChange definition: aString)'' #''refactory-changes'') #(''changeForClass: aRBClass selector: aSelector
	changes reverseDo:
			[:each |
			| change |
			change := each changeForClass: aRBClass selector: aSelector.
			change notNil ifTrue: [^change]].
	^nil'' #accessing) #(''removeInstanceVariable: variableName from: aClass
	^self
		addChange: (RBRemoveInstanceVariableChange remove: variableName from: aClass)'' #''refactory-changes'') #(''printOn: aStream
	aStream nextPutAll: name'' #printing) #(''inspect
	RefactoryBuilderInspector openOn: self'' #''user interface'') #(''flattenOnto: aCollection
	changes do: [:each | each flattenOnto: aCollection]'' #private) #(''hash
	^changes size'' #comparing) #(''= aRefactoryBuilder
	self class = aRefactoryBuilder class ifFalse: [^false].
	changes size = aRefactoryBuilder changes size ifFalse: [^false].
	changes with: aRefactoryBuilder changes
		do: [:each :change | each = change ifFalse: [^false]].
	^true'' #comparing) #(''renameClass: class to: newName
	^self addChange: (RenameClassChange rename: class name to: newName)'' #''refactory-changes'') #(''renameChangesForClass: aClassName to: newClassName
	^(self copy)
		changes: (self changes
					collect: [:each | each renameChangesForClass: aClassName to: newClassName]);
		yourself'' #accessing) #(''postCopy
	super postCopy.
	changes := changes collect: [:each | each copy]'' #copying) #(''changes: aCollection
	changes := aCollection'' #''private-inspector accessing'') #(''addInstanceVariable: variableName to: aClass
	^self addChange: (RBAddInstanceVariableChange add: variableName to: aClass)'' #''refactory-changes'') #(''compile: source in: class classified: aProtocol
	^self addChange: (RBAddMethodChange
				compile: source
				in: class
				classified: aProtocol)'' #''refactory-changes'') #(''changeForMetaclass: aSymbol selector: aSelector
	changes reverseDo:
			[:each |
			| change |
			change := each changeForMetaclass: aSymbol selector: aSelector.
			change notNil ifTrue: [^change]].
	^nil'' #accessing) #(''removeClassVariable: variableName from: aClass
	^self
		addChange: (RBRemoveClassVariableChange remove: variableName from: aClass)'' #''refactory-changes'') #(''executeNotifying: aBlock
	| undos undo |
	undos := changes collect: [:each | each executeNotifying: aBlock].
	undo := self copy.
	undo changes: undos reverse.
	^undo'' #private) #(''changes
	^changes'' #''private-inspector accessing'') #(''removeMethod: aSelector from: aClass
	^self addChange: (RBRemoveMethodChange remove: aSelector from: aClass)'' #''refactory-changes'') #(''removeClass: aClass
	^self addChange: (RBRemoveClassChange removeClassName: aClass)'' #''refactory-changes'') #(''addChange: aRefactoryChange
	changes add: aRefactoryChange.
	^aRefactoryChange'' #accessing) #(''changesSize
	^changes inject: 0 into: [:sum :each | sum + each changesSize]'' #accessing) #(''displayString
	^super displayString asText allBold'' #printing) #(''problemCount
	^self changesSize'' #accessing))) #(#RBRenameVariableChange #(#(''oldName: aString
	oldName := aString'' #private) #(''executeNotifying: aBlock
	| undo |
	undo := super executeNotifying: aBlock.
	undo
		oldName: newName;
		newName: oldName.
	^undo'' #private) #(''newName: aString
	newName := aString'' #private) #(''changeString
	^''''Rename '''' , oldName , '''' to '''' , newName'' #printing))) #(#RBRefactoryChange #(#(''flattenedChanges
	| changes |
	changes := OrderedCollection new.
	self flattenOnto: changes.
	^changes'' #private) #(''name: aString
	name := aString'' #''initialize-release'') #(''initialize'' #''initialize-release'') #(''changeForMetaclass: aSymbol selector: aSelector
	^nil'' #accessing) #(''changeString
	^self class name'' #printing) #(''changeForClass: aRBClass selector: aSelector
	^nil'' #accessing) #(''executeWithMessage: aString
	| tally controller m done |
	m := 0 asValue.
	done := 0.
	tally := self changesSize.
	controller := aString isNil
				ifTrue: [nil]
				ifFalse: [ProgressWidgetView progressOpenOn: m label: aString].
	m value: 0.
	^
	[self executeNotifying:
			[done := done + 1.
			m value: done asFloat / tally]]
			ensure: [controller notNil ifTrue: [controller closeAndUnschedule]]'' #''performing-changes'') #(''executeNotifying: aBlock
	self subclassResponsibility'' #private) #(''changes
	^Array with: self'' #accessing) #(''execute
	^self executeNotifying: []'' #''performing-changes'') #(''inspect
	^((RBCompositeRefactoryChange new)
		changes: (Array with: self);
		yourself) inspect'' #''user interface'') #(''flattenOnto: aCollection
	aCollection add: self'' #private) #(''name
	^name isNil ifTrue: [self changeString] ifFalse: [name]'' #accessing) #(''changesSize
	^1'' #accessing) #(''displayString
	^name isNil ifTrue: [self changeString] ifFalse: [name]'' #printing) #(''renameChangesForClass: aClassName to: newClassName
	"We are in the middle of performing a rename operation. If we stored
	the class name, we need to change the class name to the new
	name to perform the compiles."

	self subclassResponsibility'' #accessing))))
	do:
		[:each |
		| class |
		class := m classNamed: each first.
		each last do: [:meth | class compile: meth first classified: meth last]].
m
'
]

{ #category : #'mocking helpers' }
RBAbstractEnvTestCase >> modelWithoutRealClasses: aColOfClasses [

	| newModel classEnvironment classes |
	classes := aColOfClasses
		inject: OrderedCollection new
		into: [ :sum :each |
			self environmentOfTest at: each ifPresent: [ :class |
				sum add: class; add: class class ].
			sum ].
	classEnvironment := RBClassEnvironment classes: classes.
	newModel := self defaultNamespaceClass onEnvironment: classEnvironment not.
	newModel name: 'Test'.
	^ newModel

	
]

{ #category : #mocking }
RBAbstractEnvTestCase >> rbModelForExtractMethodTest [

	| newModel |
	newModel := self modelWithoutRealClasses: #(#MyClassA #MyClassB #MyClassC ).
	
	#('Object << #MyClassA
		slots: { #instVarName1 . #instVarName2};
		sharedVariables: { #ClassVarName1 . #ClassVarName2};
		package: ''Testing'' '
	'MyClassA << #MyClassB
		package: ''Testing'' '
	'MyClassB << #MyClassC
		package: ''Testing'' '
	) do: [ :each |
		newModel defineClass: each ].
	
	self addMethodsToModelClasses:
		#(#(#MyClassA #(
			#('bar
				^ self' #tests)
			#('name
				^ ''SomeString''' #tests)
			#('methodWithArg: anArg
				^ anArg not' #tests)
			#('methodWithArg: anArg1 andArg: anArg2
				^ (anArg1 > anArg2) not' #tests)
			#('problemCount
				^ 435' #tests)
			#('string: aString stream: aStream
				nil')
			#('myMethod
				| token |
				token := (String new: 100) writeStream.
				[ currentChar isLetter ] whileTrue: [
					token nextPut: currentChar.
					self methodWithArg: currentChar isLetter.
					self bar ].
				currentChar isLetter.
				$3 isLetter.
	 			currentChar isLetter.
				token :=  currentChar isLetter.
				token := ( currentChar isLetter) example.
				^ token contents' #tests)
			#('displayName
				| nameStream string |
				string := self name.
				nameStream := WriteStream on: (String new: 64).
					nameStream nextPutAll: string;
					nextPutAll: '' (''.
				self problemCount printOn: nameStream.
				nameStream nextPut: $).
				^nameStream contents' #tests)))
		#(#MyClassB #(
			#('someMethod
				^ instVarName2 := 3' #tests)
			#('exampleMethod
				^ (currentChar isLetter) not' #tests)
			#('dummyMethod
				(3 > self someMethod) not' #tests)
			#('anotherMethod
				|aStream|
				aStream := WriteStream on: (String new: 128).
				aStream nextPutAll: ''dfgdf'';
					nextPutAll: '' (''.
				self problemCount printOn: aStream.
				aStream nextPut: $).' #tests)))
		#(#MyClassC #(
			#('methodWithArg: anArg
				(currentChar isLetter and: [anArg isDecimal])
					ifTrue: [^ self].
				^ nil' #tests)
			#('visitMonospace: aFormat
				aFormat children do: [ :each | each accept: self ].
				self fixBlockWithoutMicText: aFormat' #tests)
			#('visitStrike: aFormat
				aFormat children do: [ :each | each accept: self ].
				self fixBlockWithoutMicText: aFormat.
				#( 4 5 2 ) do: [ :val | val children do: [ :each | each accept: self ] ]' #tests)
			#('visitParagraph: aParagraph
				^ aParagraph children do: [:each | each accept: self ]' #tests)
		)))
		in: newModel.
		
	^ newModel
]

{ #category : #mocking }
RBAbstractEnvTestCase >> rbModelForInliningMethodTest [

	| newModel |
	newModel := self modelWithoutRealClasses: #(#RBRenameClassVariableChange #RBVariableCompositeRefactoryChange #RBRenameInstanceVariableChange #RBCompositeRefactoryChange #RBRenameVariableChange #RBRefactoryChange).
	
	#(
'Object << #RBRefactoryChange
	slots: {#name};
	package: ''Refactory-Support''' 
	
'RBRefactoryChange << #RBCompositeRefactoryChange
	slots: { #changes };
	package: ''Refactory-Support''' 

'RBCompositeRefactoryChange << #RBVariableCompositeRefactoryChange
	slots: { #className . #isMeta };
	package: ''Refactory-Support''' 
	
'RBVariableCompositeRefactoryChange << #RBRenameVariableChange
	slots: { #oldName .  #newName};
	package: ''Refactory-Support''' 

'RBRenameVariableChange << #RBRenameClassVariableChange
	package: ''Refactory-Support''' 
	
'RBRenameVariableChange << #RBRenameInstanceVariableChange
	package: ''Refactory-Support''')
	do: [:each | newModel defineClass: each].

	self addMethodsToModelClasses:  
		#(
		#(#RBRenameClassVariableChange 
			#(
				#('removeOldVariable
					(RBRemoveClassVariableChange remove: oldName from: self changeClass) execute' #private) 
	
				#('addNewVariable
					(RBAddClassVariableChange add: newName to: self changeClass) execute' #private) 
		
				#('executeNotifying: aBlock
	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	self removeOldVariable.
	^undo' #private) 
	
				#('copyOldValuesToNewVariable
	| oldValue |
	oldValue := self changeClass classPool at: oldName ifAbsent: [].
	self changeClass at: newName asSymbol put: oldValue' #private))) 
	
		#(#RBVariableCompositeRefactoryChange 
			#(#('displayClassName
	^isMeta
		ifTrue: [self changeClassName , '' class'']
		ifFalse: [self changeClassName asString]' #printing) #('isMeta
	^isMeta' #private) 
			
			#('changeClass: aBehavior
	isMeta := aBehavior isMeta.
	className := isMeta
				ifTrue: [aBehavior soleInstance name]
				ifFalse: [aBehavior name]' #accessing) 
			
			#('hash
	^self changeClassName hash' #comparing) 
	
			#('changeClassName: aSymbol
	className := aSymbol.
	isMeta isNil ifTrue: [isMeta := false]' #accessing) 
			#('changeClass
	| class |
	class := Smalltalk at: self changeClassName ifAbsent: [^nil].
	^isMeta ifTrue: [class class] ifFalse: [class]' #accessing) 
			#('printOn: aStream
	aStream nextPutAll: self displayString' #printing) 
			#('changeClassName
	^className' #accessing) 
			#('= aRefactoryClassChange
	self class = aRefactoryClassChange class ifFalse: [^false].
	^className = aRefactoryClassChange changeClassName
		and: [isMeta = aRefactoryClassChange isMeta]' #comparing))) 
		
	#(#RBRenameInstanceVariableChange 
		#(
			#('removeOldVariable
	(RBRemoveInstanceVariableChange remove: oldName from: self changeClass)
		execute' #private) 
			#('addNewVariable
	(RBAddInstanceVariableChange add: newName to: self changeClass) execute' #private) #('executeNotifying: aBlock
	| undo |
	self addNewVariable.
	self copyOldValuesToNewVariable.
	undo := super executeNotifying: aBlock.
	self removeOldVariable.
	^undo' #private) 
	
			#('copyOldValuesToNewVariable
	| newIndex oldIndex |
	oldIndex := self changeClass allInstVarNames indexOf: oldName asString.
	newIndex := self changeClass allInstVarNames indexOf: newName asString.
	self changeClass withAllSubclasses do:
			[:each |
			each allInstances
				do: [:inst | each instVarAt: newIndex put: (each instVarAt: oldIndex)]]' #private))) 

	#(#RBCompositeRefactoryChange 
		#(#('compile: source in: class
			^self addChange: (RBAddMethodChange compile: source in: class)' #'refactory-changes') 
			#('addClassVariable: variableName to: aClass
	^self addChange: (RBAddClassVariableChange add: variableName to: aClass)' #'refactory-changes') 
			#('addChangeFirst: aRefactoryChange
	changes addFirst: aRefactoryChange.
	^aRefactoryChange' #accessing) 
	
			#('removeChange: aChange
	changes remove: aChange ifAbsent: []' #'private-inspector accessing') 
			#('addPool: aPoolVariable to: aClass
	^self addChange: (RBAddPoolVariableChange add: aPoolVariable to: aClass)' #'refactory-changes') 
			#('initialize
	super initialize.
	changes := OrderedCollection new' #'initialize-release') 
			#('defineClass: aString
	^self addChange: (RBAddClassChange definition: aString)' #'refactory-changes') 
			#('changeForClass: aRBClass selector: aSelector
	changes reverseDo:
			[:each |
			| change |
			change := each changeForClass: aRBClass selector: aSelector.
			change notNil ifTrue: [^change]].
	^nil' #accessing) 
			#('removeInstanceVariable: variableName from: aClass
	^self
		addChange: (RBRemoveInstanceVariableChange remove: variableName from: aClass)' #'refactory-changes') 
			#('printOn: aStream
	aStream nextPutAll: name' #printing) 
			#('inspect
	RefactoryBuilderInspector openOn: self' #'user interface') 
			#('flattenOnto: aCollection
	changes do: [:each | each flattenOnto: aCollection]' #private) 
			#('hash
	^changes size' #comparing) 
			#('= aRefactoryBuilder
	self class = aRefactoryBuilder class ifFalse: [^false].
	changes size = aRefactoryBuilder changes size ifFalse: [^false].
	changes with: aRefactoryBuilder changes
		do: [:each :change | each = change ifFalse: [^false]].
	^true' #comparing) 
			#('renameClass: class to: newName
	^self addChange: (RenameClassChange rename: class name to: newName)' #'refactory-changes') 
			#('renameChangesForClass: aClassName to: newClassName
	^(self copy)
		changes: (self changes
					collect: [:each | each renameChangesForClass: aClassName to: newClassName]);
		yourself' #accessing) 
			#('postCopy
	super postCopy.
	changes := changes collect: [:each | each copy]' #copying) 
			#('changes: aCollection
	changes := aCollection' #'private-inspector accessing') 
			#('addInstanceVariable: variableName to: aClass
	^self addChange: (RBAddInstanceVariableChange add: variableName to: aClass)' #'refactory-changes') 
		#('compile: source in: class classified: aProtocol
	^self addChange: (RBAddMethodChange
				compile: source
				in: class
				classified: aProtocol)' #'refactory-changes') 
		
		#('changeForMetaclass: aSymbol selector: aSelector
	changes reverseDo:
			[:each |
			| change |
			change := each changeForMetaclass: aSymbol selector: aSelector.
			change notNil ifTrue: [^change]].
	^nil' #accessing) 
		
		#('removeClassVariable: variableName from: aClass
	^self
		addChange: (RBRemoveClassVariableChange remove: variableName from: aClass)' #'refactory-changes') 
		
		#('executeNotifying: aBlock
	| undos undo |
	undos := changes collect: [:each | each executeNotifying: aBlock].
	undo := self copy.
	undo changes: undos reverse.
	^undo' #private) 
	
		#('changes
	^changes' #'private-inspector accessing') 
		#('removeMethod: aSelector from: aClass
	^self addChange: (RBRemoveMethodChange remove: aSelector from: aClass)' #'refactory-changes') 
		#('removeClass: aClass
	^self addChange: (RBRemoveClassChange removeClassName: aClass)' #'refactory-changes') 
		#('addChange: aRefactoryChange
	changes add: aRefactoryChange.
	^aRefactoryChange' #accessing) 
		#('changesSize
	^changes inject: 0 into: [:sum :each | sum + each changesSize]' #accessing) 
		#('displayString
	^super displayString asText allBold' #printing) 
		#('problemCount
	^self changesSize' #accessing))) 

#(#RBRenameVariableChange #(#('oldName: aString
	oldName := aString' #private) #('executeNotifying: aBlock
	| undo |
	undo := super executeNotifying: aBlock.
	undo
		oldName: newName;
		newName: oldName.
	^undo' #private) #('newName: aString
	newName := aString' #private) 
	
	#('changeString
	^''Rename '' , oldName , '' to '' , newName' #printing))) 
	
#(#RBRefactoryChange #(#('flattenedChanges
	| changes |
	changes := OrderedCollection new.
	self flattenOnto: changes.
	^changes' #private) #('name: aString
	name := aString' #'initialize-release') 
	
	#('initialize' #'initialize-release') 
	
	#('changeForMetaclass: aSymbol selector: aSelector
	^nil' #accessing) 
	
	#('changeString
	^self class name' #printing) 
	
	#('changeForClass: aRBClass selector: aSelector
	^nil' #accessing) 
	
	#('executeWithMessage: aString
	| tally controller m done |
	m := 0 asValue.
	done := 0.
	tally := self changesSize.
	controller := aString isNil
				ifTrue: [nil]
				ifFalse: [ProgressWidgetView progressOpenOn: m label: aString].
	m value: 0.
	^
	[self executeNotifying:
			[done := done + 1.
			m value: done asFloat / tally]]
			ensure: [controller notNil ifTrue: [controller closeAndUnschedule]]' #'performing-changes') 
			
	#('executeNotifying: aBlock
	self subclassResponsibility' #private) 
	
	#('changes
	^Array with: self' #accessing) 
	
	#('execute
	^self executeNotifying: []' #'performing-changes') 
	
	#('inspect
	^((RBCompositeRefactoryChange new)
		changes: (Array with: self);
		yourself) inspect' #'user interface') 
		
		#('flattenOnto: aCollection
	aCollection add: self' #private) 
		#('name
	^name isNil ifTrue: [self changeString] ifFalse: [name]' #accessing) #('changesSize
	^1' #accessing) 
	
		#('displayString
	^name isNil ifTrue: [self changeString] ifFalse: [name]' #printing) 
		#('renameChangesForClass: aClassName to: newClassName
	"We are in the middle of performing a rename operation. If we stored
	the class name, we need to change the class name to the new
	name to perform the compiles."

	self subclassResponsibility' #accessing))))
	in: newModel.
	
	^ newModel
]

{ #category : #mocking }
RBAbstractEnvTestCase >> rbModelForVariableTest [

	| newModel |
	newModel := self modelWithoutRealClasses: #(Foo Bar).
	
	#('Object << #Foo
		slots: { #instVarName1 . #instVarName2};
		sharedVariables: { #ClassVarName1 . #ClassVarName2 };
		package: ''Testing'' '
	'Foo << #Bar
		package: ''Testing'' '
	) do: [ :each | newModel defineClass: each ].

	self addMethodsToModelClasses:  
		#(#(#Bar #(
			#('instVarName1
				^1242321' #tests)
			#('foo
				instVarName1 := instVarName1 + instVarName2 + ClassVarName1' #tests)))
		#(#Foo #(
			#('foo
				^instVarName2 := 3' #tests)
			#('bar
				"Add one to instVarName1"
				instVarName1 := instVarName1 + 1' #tests)
			#('classVarName1
				^ClassVarName1' #tests)
			#('instVarName1: anObject
				^anObject' #tests)
			#('asdf
				^self classVarName1: (ClassVarName1 := ClassVarName1 + 1)' #tests)
			#('instVarName2
				^instVarName2' #tests)
			#('instVarName2: anObject
				instVarName2 := anObject' #tests)
			#('classVarName1: anObject
				^ClassVarName1 := anObject' #tests)))
		#('Bar class' #(
			#('classVarName2: anObject
				ClassVarName2 := anObject' #tests)
			#('classVarName2
				^ClassVarName2' #tests)))
		#('Foo class' #(
			#('foo
				^ClassVarName1 := ClassVarName1 * ClassVarName1 * ClassVarName1' #tests))))
	in: newModel.
	
	^ newModel
]

{ #category : #running }
RBAbstractEnvTestCase >> setUp [
	super setUp.
	testingEnvironment := Smalltalk globals
]

{ #category : #accessing }
RBAbstractEnvTestCase >> testingEnvironment: anObject [
	testingEnvironment := anObject
]
