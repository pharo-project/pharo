"
I'm an InstructionClient that tries to guess the type of a given instance variable name of a class. 
"
Class {
	#name : #NECVarTypeGuesser,
	#superclass : #InstructionClient,
	#instVars : [
		'types',
		'receiverClass',
		'variableName',
		'found',
		'currentMethod',
		'hasSend',
		'contextCount'
	],
	#category : #'NECompletion-TypeGuesser'
}

{ #category : #private }
NECVarTypeGuesser class >> classFrom: className [

	^ Smalltalk globals at: className asSymbol ifAbsent: [  ]
]

{ #category : #private }
NECVarTypeGuesser class >> getClassFromTypeSuggestingName: aString [
	"Try to return the class represented by aString. This method uses as heuristics either the fact that aString represents directly a class or that the className starts at the first uppercase letter occurrence."

	"(self getClassFromTypeSuggestingName: 'Point')
	>>> Point"

	"(self getClassFromTypeSuggestingName: 'aPoint')
	>>> Point"

	| firstUppercaseLetter className  aClass |
	className := String streamContents: [ : aStream |
		aStream nextPut: aString first asUppercase.
		aStream nextPutAll: (aString copyFrom: 2 to: aString size)]. 
	aClass := self classFrom: className.
	(aClass isKindOf: Class)
		ifTrue: [ ^ aClass ].
	firstUppercaseLetter := aString indexOfFirstUppercaseCharacter.
	className := firstUppercaseLetter > 1
		ifTrue: [ aString copyFrom: firstUppercaseLetter to: aString size ]
		ifFalse: [ ^ nil ].
	aClass := self classFrom: className.
	(aClass isKindOf: Class)
		ifTrue: [ ^ aClass ].
	^ nil
]

{ #category : #'instance creation' }
NECVarTypeGuesser class >> variableName: aString class: aClass [ 
	^self variableName: aString source: nil class: aClass 
]

{ #category : #'instance creation' }
NECVarTypeGuesser class >> variableName: aString source: sourceString class: aClass [ 
	| newInstance |
	newInstance := self basicNew initialize.
	newInstance 
		setVariableName: aString
		source: sourceString
		class: aClass.
	^ newInstance
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> blockReturnTop [
	contextCount := contextCount - 1
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> computeVarType [
	| info tempNames name node |
	types ifEmpty: [ ^ nil ].
	info := types first.
	(hasSend and: [ info isDefinedByMessageSend not ])
		ifTrue: [ info type: nil.
			^ info ].
	info isDefinedByTemporary ifFalse: [ ^ info ].
	node := receiverClass compiler parse: currentMethod sourceCode.
	self flag: #pharoFixMe.	"I think tempNames is a bad name if we also take the arguments names? So, should we rename it or should we not take arguments names?"
	tempNames := node argumentNames , node temporaryNames.
	name := tempNames at: info temporaryOffset + 1.
	info type: (self class getClassFromTypeSuggestingName: name).
	^ info
]

{ #category : #initialization }
NECVarTypeGuesser >> initialize [
	super initialize.
	types := OrderedCollection new.
	hasSend := false.
	contextCount := 0
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> interpretNextInstructionUsing: aScanner [ 
	found := false.
	aScanner interpretNextInstructionFor: self.
	^found

]

{ #category : #public }
NECVarTypeGuesser >> methodRefs [
	^ #()
]

{ #category : #public }
NECVarTypeGuesser >> perform [
	| infos infosOfMethod methodRefs |
	variableName isEmpty
		ifTrue: [^ nil].
	methodRefs := self methodRefs.
	methodRefs
		ifNil: [^ nil]. 
	methodRefs isBehavior
		ifTrue: [^ methodRefs].
	contextCount := 0.
	infos := SortedCollection
				sortBlock: [:a :b | a priority <= b priority].
	methodRefs
		do: [:each | 
			self prepare: each.
			infosOfMethod := self typeOfVarIn: each compiledMethod.
			infos addAll: infosOfMethod].
	infos notEmpty
		ifTrue: [^ infos first type].
	^ nil
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> popIntoLiteralVariable: anAssociation [ 
	"Remove Top Of Stack And Store Into Literal Variable bytecode."
	self reset
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> popIntoTemporaryVariable: offset [ 
	"Remove Top Of Stack And Store Into Temporary Variable bytecode."
	self reset

]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> prepare: aCompiledMethod [ 
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> pushActiveContext [
	"Push Active Context On Top Of Its Own Stack bytecode."
	contextCount := contextCount + 1
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> pushConstant: value [ 
	"Push Constant, value, on Top Of Stack bytecode."
	| info |
	contextCount > 0 ifTrue: [ ^ self ].
	value ifNotNil: 
		[ info := NECTypeInfo definedByLiteral: value class.
		types add: info ]
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> pushLiteralVariable: anAssociation [ 
	"Push Contents Of anAssociation On Top Of Stack bytecode."
	| info |
	contextCount > 0 ifTrue:[^self].

	info := NECTypeInfo definedByMessageSend: (anAssociation value isBehavior
		ifTrue: [ anAssociation value ]
		ifFalse: [ anAssociation value class ]).
	types add: info
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> pushTemporaryVariable: offset [ 
	"Push Contents Of Temporary Variable Whose Index Is the  
	argument, offset, On Top Of Stack bytecode."
	| info |
		contextCount > 0 ifTrue:[^self].

	info := NECTypeInfo definedByTemporaryVar: offset.
	types add: info
]

{ #category : #private }
NECVarTypeGuesser >> reset [
	contextCount > 0 ifTrue:[^self].
	types reset.
	hasSend := false.
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> send: selector super: supered numArgs: numberArguments [ 
	"Send Message With Selector, selector, bytecode. The argument,  
	supered, indicates whether the receiver of the message is specified with 
	'super' in the source method. The arguments of the message are found  
	in  
	the top numArguments locations on the stack and the receiver just  
	below them."
	contextCount > 0 ifTrue:[^self].

	hasSend := true
]

{ #category : #'instance creation' }
NECVarTypeGuesser >> setVariableName: aString source: aSourceString class: aClass [ 
	variableName := aString.
	receiverClass := aClass
]

{ #category : #'bytecode decoding' }
NECVarTypeGuesser >> typeOfVarIn: aMethod [ 
	"Answer whether the receiver references an instance variable."
	| scanner end type infos |
	scanner := InstructionStream on: aMethod.
	end := scanner method endPC.
	currentMethod := aMethod.
	infos := OrderedCollection new.
	[ scanner pc <= end ] whileTrue: 
		[ (self interpretNextInstructionUsing: scanner) ifTrue: 
			[ type := self computeVarType.
			type ifNotNil: [ infos add: type ] ] ].
	^ infos
]
