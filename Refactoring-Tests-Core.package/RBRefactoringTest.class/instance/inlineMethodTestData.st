private
inlineMethodTestData
^ '
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
#(''Object subclass: #RBRefactoryChange
	instanceVariableNames: ''''name ''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Refactory-Support'''''' ''RBRefactoryChange subclass: #RBCompositeRefactoryChange
	instanceVariableNames: ''''changes ''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Refactory-Support'''''' ''RBCompositeRefactoryChange subclass: #RBVariableCompositeRefactoryChange
	instanceVariableNames: ''''className isMeta ''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Refactory-Support'''''' ''RBVariableCompositeRefactoryChange subclass: #RBRenameVariableChange
	instanceVariableNames: ''''oldName newName ''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Refactory-Support'''''' ''RBRenameVariableChange subclass: #RBRenameClassVariableChange
	instanceVariableNames: ''''''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Refactory-Support'''''' ''RBRenameVariableChange subclass: #RBRenameInstanceVariableChange
	instanceVariableNames: ''''''''
	classVariableNames: ''''''''
	poolDictionaries: ''''''''
	category: ''''Refactory-Support'''''') 
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