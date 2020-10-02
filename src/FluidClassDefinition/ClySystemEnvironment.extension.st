Extension { #name : #ClySystemEnvironment }

{ #category : #'*FluidClassDefinition' }
ClySystemEnvironment >> compileANewClassFrom: aString notifying: aController startingFrom: aClass [
	" Copied from Browser "

	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aController."

	| oldClass class newClassName defTokens keywdIx classCompiler |
	"for now make it work!"
	Slot showSlotClassDefinition
		ifTrue: [ ^ self defineNewClassFrom: aString notifying: aController  startingFrom: aClass ].
	
	oldClass := aClass.
	defTokens := aString findTokens: Character separators.
	((defTokens first = 'Trait' and: [ defTokens second = 'named:' ])
		or: [ defTokens second = 'classTrait' ])
		ifTrue: [ ^ self
				defineTrait: aString
				notifying: aController
				startingFrom: aClass ].
	keywdIx := defTokens findFirst: [ :x | x beginsWith: 'category' ].
	keywdIx := defTokens findFirst: [ :x | '*subclass*' match: x ].
	newClassName := (defTokens at: keywdIx + 1) copyWithoutAll: '#()'.
	((oldClass isNil
		or: [ oldClass instanceSide name asString ~= newClassName ])
		and: [ self includesClassNamed: newClassName asSymbol ])
		ifTrue: [ "Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self
				confirm:
					((newClassName
						,
							' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText
						makeBoldFrom: 1
						to: newClassName size))
				ifFalse: [ ^ nil ] ].
	"Use oldClass superclass for defining oldClass since oldClass superclass knows the definerClass of oldClass."
	oldClass
		ifNil: [ classCompiler := self defaultClassCompiler ]
		ifNotNil: [ classCompiler := oldClass superclass subclassDefinerClass new ].
	[class := classCompiler
		source: aString;
		requestor: aController;
		logged: true;
		evaluate] on: OCUndeclaredVariableWarning do: [ :ex | 
			"we are only interested in class definitions"
			ex compilationContext noPattern ifFalse: [ex pass ].
 			"Undeclared Vars should not lead to the standard dialog to define them but instead should not accept"
 			self inform: 'Undeclared Variable in Class Definition' . ^nil ].
	^ class isBehavior
		ifTrue: [ class ]
		ifFalse: [ nil ]
]

{ #category : #'*FluidClassDefinition' }
ClySystemEnvironment >> defineNewClassFrom: aString notifying: aController startingFrom: aClass [
	
	| class classCompiler |
	"the logic of erasing an existing class will be added back later."
	aClass
		ifNil: [ classCompiler := self defaultClassCompiler ]
		ifNotNil: [ classCompiler := aClass superclass subclassDefinerClass new ].
	[class := classCompiler
		"for now a super ugly patch"
		source: 'ShiftClassInstaller new makeWithBuilder: (',  aString,') shiftClassBuilder ';
		requestor: aController;
		logged: true;
		evaluate] on: OCUndeclaredVariableWarning do: [ :ex | 
			"we are only interested in class definitions"
			ex compilationContext noPattern ifFalse: [ex pass ].
 			"Undeclared Vars should not lead to the standard dialog to define them but instead should not accept"
 			self inform: 'Undeclared Variable in Class Definition' . ^nil ].
	^ class isBehavior
		ifTrue: [ class ]
		ifFalse: [ nil ]
]
