"
Preferences for Shout
"
Class {
	#name : #SHPreferences,
	#superclass : #Object,
	#classVars : [
		'CustomStyleTable',
		'Groups'
	],
	#classInstVars : [
		'enabled'
	],
	#category : #'Shout-Styling'
}

{ #category : #private }
SHPreferences class >> applyStyle [
	| table |
	table := Groups values flatCollect: [:group | group styleForTable].
	SHRBTextStyler styleTable: table.
]

{ #category : #'accessing-styles' }
SHPreferences class >> argsStyle [
	^ Groups at: #args
]

{ #category : #'accessing-styles' }
SHPreferences class >> argsStyle: aGroupStyle [ 
	^ Groups at: #args put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> commentStyle [
	^ Groups at: #comment
]

{ #category : #'accessing-styles' }
SHPreferences class >> commentStyle: aGroupStyle [ 
	^ Groups at: #comment put: aGroupStyle
]

{ #category : #private }
SHPreferences class >> customStyleTable [
	^ CustomStyleTable
]

{ #category : #private }
SHPreferences class >> customStyleTable: anArray [ 
	CustomStyleTable := anArray.
	self initializeGroups
]

{ #category : #'accessing-styles' }
SHPreferences class >> defaultStyle [
	^ Groups at: #default
]

{ #category : #'accessing-styles' }
SHPreferences class >> defaultStyle: aGroupStyle [ 
	^ Groups at: #default put: aGroupStyle
]

{ #category : #accessing }
SHPreferences class >> enabled [
	^ enabled
		ifNil: [enabled := true]
]

{ #category : #accessing }
SHPreferences class >> enabled: aBoolean [ 
	enabled := aBoolean
]

{ #category : #'accessing-styles' }
SHPreferences class >> errorStyle [
	^ Groups at: #error
]

{ #category : #'accessing-styles' }
SHPreferences class >> errorStyle: aGroupStyle [ 
	^ Groups at: #error put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> globalVarStyle [
	^ Groups at: #globalVar
]

{ #category : #'accessing-styles' }
SHPreferences class >> globalVarStyle: aGroupStyle [ 
	^ Groups at: #globalVar put: aGroupStyle
]

{ #category : #initialization }
SHPreferences class >> initialize [
	self customStyleTable: SHRBTextStyler defaultStyleTable
]

{ #category : #initialization }
SHPreferences class >> initializeGroups [
	Groups := Dictionary new.
	#(#default #(#default ) #reservedWords #(#self #super #true #false #nil #thisContext ) #primitiveTypes #(#'$' #character #integer #number #- #symbol #stringSymbol #literalArray #string #unfinishedString ) #selectorPatterns #(#patternKeyword #patternBinary #patternUnary ) #messageSends #(#keyword #binary #unary #incompleteKeyword #incompleteBinary #incompleteUnary ) #primitiveOrModule #(#primitive #externalFunctionCallingConvention #module #externalCallType #externalCallTypePointerIndicator #primitiveOrExternalCallStart #primitiveOrExternalCallEnd ) #args #(#patternArg #methodArg #blockPatternArg #blockArg #argument ) #variable #(#blockTempVar #blockPatternTempVar #workspaceVar #tempVar #patternTempVar #incompleteIdentifier ) #instanceVar #(#instVar #classVar ) #globalVar #(#globalVar #poolConstant ) #comment #(#comment #unfinishedComment ) #error #(#invalid #excessCode #undefinedKeyword #undefinedBinary #undefinedUnary #undefinedIdentifier ) #syntax #(#assignment #ansiAssignment #literal #return #blockArgColon #leftParenthesis #rightParenthesis #leftParenthesis1 #rightParenthesis1 #leftParenthesis2 #rightParenthesis2 #leftParenthesis3 #rightParenthesis3 #leftParenthesis4 #rightParenthesis4 #leftParenthesis5 #rightParenthesis5 #leftParenthesis6 #rightParenthesis6 #leftParenthesis7 #rightParenthesis7 #blockStart #blockEnd #blockStart1 #blockEnd1 #blockStart2 #blockEnd2 #blockStart3 #blockEnd3 #blockStart4 #blockEnd4 #blockStart5 #blockEnd5 #blockStart6 #blockEnd6 #blockStart7 #blockEnd7 #arrayStart #arrayEnd #arrayStart1 #arrayEnd1 #byteArrayStart #byteArrayEnd #byteArrayStart1 #byteArrayEnd1 #leftBrace #rightBrace #cascadeSeparator #statementSeparator #methodTempBar #blockTempBar #blockArgsBar ) )
		pairsDo: [:gname :tokens | Groups
				at: gname
				put: (SHStyleElement withTokens: tokens)]
]

{ #category : #'accessing-styles' }
SHPreferences class >> instanceVarStyle [
	^ Groups at: #instanceVar
]

{ #category : #'accessing-styles' }
SHPreferences class >> instanceVarStyle: aGroupStyle [ 
	^ Groups at: #instanceVar put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> messageSendsStyle [
	^ Groups at: #messageSends
]

{ #category : #'accessing-styles' }
SHPreferences class >> messageSendsStyle: aGroupStyle [ 
	^ Groups at: #messageSends put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> primitiveOrModuleStyle [
	^ Groups at: #primitiveOrModule
]

{ #category : #'accessing-styles' }
SHPreferences class >> primitiveOrModuleStyle: aGroupStyle [ 
	^ Groups at: #primitiveOrModule put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> primitiveTypesStyle [
	^ Groups at: #primitiveTypes
]

{ #category : #'accessing-styles' }
SHPreferences class >> primitiveTypesStyle: aGroupStyle [ 
	^ Groups at: #primitiveTypes put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> reservedWordsStyle [
	^ Groups at: #reservedWords
]

{ #category : #'accessing-styles' }
SHPreferences class >> reservedWordsStyle: aGroupStyle [ 
	^ Groups at: #reservedWords put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> selectorPatternsStyle [
	^ Groups at: #selectorPatterns
]

{ #category : #'accessing-styles' }
SHPreferences class >> selectorPatternsStyle: aGroupStyle [ 
	^ Groups at: #selectorPatterns put: aGroupStyle
]

{ #category : #settings }
SHPreferences class >> setStyleTable: anArray [
	SHRBTextStyler styleTable: anArray.
]

{ #category : #settings }
SHPreferences class >> setStyleTableNamed: aString [
	self setStyleTable: (((Pragma allNamed: #styleTable: in: SHRBTextStyler class)
		detect: [ :each | (each argumentAt: 1) = aString ])
		method 
			valueWithReceiver: SHRBTextStyler class 
			arguments: #())
]

{ #category : #settings }
SHPreferences class >> settingsOn: aBuilder [ 
	<systemsettings>
	(aBuilder setting: #'Syntax Highlighting') 
		target: self;
		dialog: [ self styleTableRow ];
		parentName: #codeBrowsing;
		description: 'Syntax Highlighting As You Type: Enable syntax highlighting in browsers, debuggers and workspaces and set patterns style.';
		selector: #enabled;
		default: true;
		iconName: #smallConfigurationIcon;
		with: [
			(aBuilder setting: #selectorPatternsStyle) 
				label: 'Selector Patterns';
				default: (SHStyleElement emphasis: #bold tokens: #(#patternKeyword #patternBinary #patternUnary));
				description: 'Selector patterns in method pane'.
			(aBuilder setting: #errorStyle)			 
				default: (SHStyleElement color: Color red tokens: #(#invalid #excessCode #undefinedKeyword #undefinedBinary #undefinedUnary #undefinedIdentifier));
				label: 'Syntactic error';
				description: 'Invalid and undefined code' .
			(aBuilder setting: #argsStyle) 
				label: 'Parameters';
				default: (SHStyleElement color: (Color r: 0.0 g: 0.0 b: 0.5004887585532747 alpha: 1.0) tokens: #(#patternArg #methodArg #blockPatternArg #blockArg #argument));
				description: 'Parameters in patterns, message sends, and blocks' .
			(aBuilder setting: #commentStyle) 
				label: 'Comments';
				default: (SHStyleElement color: (Color r: 0.4203323558162268 g: 0.4203323558162268 b: 0.4203323558162268 alpha: 1.0) tokens: #(#comment #unfinishedComment));
				description: 'Comments in code pane' .
			(aBuilder setting: #defaultStyle) 
				label: 'Default' ;
				default: (SHStyleElement color: Color black tokens: #(#default));
				description: 'Default style' .
			(aBuilder setting: #globalVarStyle) 
				label: 'Global variables' ;
				default: (SHStyleElement color: (Color r: 0.0 g: 0.0 b: 0.5004887585532747 alpha: 1.0) tokens: #(#globalVar #poolConstant));
				description: 'References to global variables, including classes' .
			(aBuilder setting: #instanceVarStyle) 
				label: 'Instance/class variables' ;
				default: (SHStyleElement color: (Color r: 0.0 g: 0.0 b: 0.5004887585532747 alpha: 1.0) tokens: #(#instVar #classVar));
				description: 'References to instance and class variables' .
			(aBuilder setting: #messageSendsStyle) 
				default: (SHStyleElement color: Color black tokens: #(#keyword #binary #unary #incompleteKeyword #incompleteBinary #incompleteUnary));
				label: 'Message sends' ;
				description: 'Message sends' .
			(aBuilder setting: #primitiveOrModuleStyle) 
				label: 'Primitive or module' ;
				default: (SHStyleElement color: (Color r: 0.0 g: 0.5004887585532747 b: 0.0 alpha: 1.0) tokens: #(#primitive #externalFunctionCallingConvention #module #externalCallType #externalCallTypePointerIndicator #primitiveOrExternalCallStart #primitiveOrExternalCallEnd));
				description: 'Primitive or module' .
			(aBuilder setting: #primitiveTypesStyle) 
				label: 'Primitive types';
				default: (SHStyleElement color: (Color r: 0.5004887585532747 g: 0.0 b: 0.0 alpha: 1.0) tokens: #(#'$' #character #integer #number #- #symbol #stringSymbol #literalArray #string #unfinishedString));
				description: 'Literal data' .
			(aBuilder setting: #reservedWordsStyle) 
				label: 'Reserved words';
				default: (SHStyleElement color: (Color r: 0.0 g: 0.5004887585532747 b: 0.5004887585532747 alpha: 1.0) tokens: #(#self #super #true #false #nil #thisContext));
				description: 'Reserved words of the Smalltalk language' .
			(aBuilder setting: #syntaxStyle) 
				label: 'Syntax';
				default: (SHStyleElement tokens: #(#assignment #ansiAssignment #literal #return #blockArgColon #leftParenthesis #rightParenthesis #leftParenthesis1 #rightParenthesis1 #leftParenthesis2 #rightParenthesis2 #leftParenthesis3 #rightParenthesis3 #leftParenthesis4 #rightParenthesis4 #leftParenthesis5 #rightParenthesis5 #leftParenthesis6 #rightParenthesis6 #leftParenthesis7 #rightParenthesis7 #blockStart #blockEnd #blockStart1 #blockEnd1 #blockStart2 #blockEnd2 #blockStart3 #blockEnd3 #blockStart4 #blockEnd4 #blockStart5 #blockEnd5 #blockStart6 #blockEnd6 #blockStart7 #blockEnd7 #arrayStart #arrayEnd #arrayStart1 #arrayEnd1 #byteArrayStart #byteArrayEnd #byteArrayStart1 #byteArrayEnd1 #leftBrace #rightBrace #cascadeSeparator #statementSeparator #methodTempBar #blockTempBar #blockArgsBar));
				description: 'Any other syntactic element' .
			(aBuilder setting: #variableStyle) 
				label: 'Variable';
				default: (SHStyleElement color: (Color r: 0.0 g: 0.0 b: 0.5004887585532747 alpha: 1.0) tokens: #(#blockTempVar #blockPatternTempVar #workspaceVar #tempVar #patternTempVar #incompleteIdentifier));
				description: 'Temporary variable'  ]
]

{ #category : #settings }
SHPreferences class >> styleTableRow [
	| allStyles |
	
	allStyles := Pragma  allNamed: #styleTable: in: SHRBTextStyler class.
	^Smalltalk ui theme 
		newRowIn: self currentWorld 
		for: (
			{ Smalltalk ui theme buttonLabelForText: 'Predefined styles: ' translated }, 
			(allStyles collect: [ :eachPragma | 
				(Smalltalk ui theme 
					newButtonIn: self currentWorld 
						for:  self
						getState: nil 
						action: #setStyleTableNamed:  
						arguments:  { eachPragma argumentAt: 1 }
						getEnabled: nil 
						getLabel: nil
						help: ('Change style table to ', (eachPragma argumentAt: 1)) translated)
					label: (eachPragma argumentAt: 1);
					yourself ] ))
]

{ #category : #'accessing-styles' }
SHPreferences class >> syntaxStyle [
	^ Groups at: #syntax
]

{ #category : #'accessing-styles' }
SHPreferences class >> syntaxStyle: aGroupStyle [ 
	^ Groups at: #syntax put: aGroupStyle
]

{ #category : #'accessing-styles' }
SHPreferences class >> variableStyle [
	^ Groups at: #variable
]

{ #category : #'accessing-styles' }
SHPreferences class >> variableStyle: aGroupStyle [ 
	^ Groups at: #variable put: aGroupStyle
]
