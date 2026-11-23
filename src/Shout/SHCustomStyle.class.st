"
I store all the style information associated with 'Custom' style
"
Class {
	#name : 'SHCustomStyle',
	#superclass : 'Object',
	#classInstVars : [
		'groups'
	],
	#category : 'Shout-Styling',
	#package : 'Shout',
	#tag : 'Styling'
}

{ #category : 'settings - accessing' }
SHCustomStyle class >> argsStyle [

	^ self styleForGroup: 'args'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> argsStyle: aStyle [

	^ self style: aStyle forGroup: 'args'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> commentStyle [

	^ self styleForGroup: 'comment'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> commentStyle: aStyle [ 

	^ self style: aStyle forGroup: 'comment'
]

{ #category : 'class initialization' }
SHCustomStyle class >> copyFromCurrentStyle [
	SHRBTextStyler styleTable do: [ :row |
			| token |
			token := row first.
			groups detect: [ :entry | token = (entry at: 'tokens') first ] ifFound: [ :entry |
					| colorDesc emphasis |
					colorDesc := row second.
					emphasis := #normal.
					row size >= 3 ifTrue: [ emphasis := row third ].
					(emphasis isKindOf: Symbol) ifFalse: [ emphasis := (' ' join: emphasis) asSymbol ].
					(entry at: 'style')
						color: ((Color colorFrom: colorDesc) ifNil: [ Color transparent ]) ;
						emphasis: emphasis ] ].
]

{ #category : 'private' }
SHCustomStyle class >> defaultColor [

	^ Color r: 0.0 g: 0.75 b: 1.0
]

{ #category : 'private' }
SHCustomStyle class >> defaultEmphasis [

	^ #normal
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> defaultStyle [

	^ self styleForGroup: 'default'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> defaultStyle: aStyle [ 

	^ self style: aStyle forGroup: 'default'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> errorStyle [

	^ self styleForGroup: 'error'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> errorStyle: aStyle [ 

	^ self style: aStyle forGroup: 'error'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> globalVarStyle [

	^ self styleForGroup: 'globalVar'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> globalVarStyle: aStyle [ 

	^ self style: aStyle forGroup: 'globalVar'
]

{ #category : 'private' }
SHCustomStyle class >> groupDefaults [

	^ Dictionary newFrom: {
			  ('default' -> {
				   ('name' -> 'default').
				   ('tokens' -> #( 'default' )).
				   ('label' -> 'Default').
				   ('description' -> 'Default style') } asDictionary).
			  ('reservedWords' -> {
				   ('name' -> 'reservedWords').
				   ('tokens' -> #( 'self' 'super' 'true' 'false' 'nil' 'thisContext' )).
				   ('label' -> 'Reserved words').
				   ('description' -> 'Reserved words of the Smalltalk language') }
				   asDictionary).
			  ('primitiveTypes' -> {
				   ('name' -> 'primitiveTypes').
				   ('description' -> 'Literal data').
				   ('tokens' -> #( 'character' 'number' 'symbol' 'string' )).
				   ('label' -> 'Primitive types') } asDictionary).
			  ('selectorPatterns' -> {
				   ('name' -> 'selectorPatterns').
				   ('tokens' -> #( 'patternSelector' )).
				   ('label' -> 'Selector Patterns').
				   ('description' -> 'Selector patterns in method pane') }
				   asDictionary).
			  ('messageSends' -> {
				   ('name' -> 'messageSends').
				   ('tokens' -> #( 'selector' 'incompleteSelector' )).
				   ('label' -> 'Message sends') } asDictionary).
			  ('args' -> {
				   ('name' -> 'args').
				   ('label' -> 'Parameters').
				   ('description'
				    -> 'Parameters in patterns, message sends, and blocks').
				   ('tokens'
				    -> #( 'patternArg' 'blockPatternArg' 'blockArg' 'argument' )) }
				   asDictionary).
			  ('variable' -> {
				   ('name' -> 'variable').
				   ('label' -> 'Variable').
				   ('description' -> 'Temporary variable').
				   ('tokens'
				    ->
					    #( 'blockTempVar' 'blockPatternTempVar' 'tempVar' 'patternTempVar'
					       'incompleteIdentifier' )) } asDictionary).
			  ('instanceVar' -> {
				   ('name' -> 'instanceVar').
				   ('label' -> 'Instance/class variables').
				   ('description' -> 'References to instance and class variables').
				   ('tokens' -> #( 'instVar' 'classVar' )) } asDictionary).
			  ('globalVar' -> {
				   ('name' -> 'globalVar').
				   ('label' -> 'Global variables').
				   ('description'
				    -> 'References to global variables, including classes').
				   ('tokens' -> #( 'globalVar' 'poolConstant' )) } asDictionary).
			  ('comment' -> {
				   ('name' -> 'comment').
				   ('label' -> 'Comments').
				   ('descripiton' -> 'Comments in code pane').
				   ('tokens' -> #( 'comment' )) } asDictionary).
			  ('error' -> {
				   ('name' -> 'error').
				   ('label' -> 'Syntactic error').
				   ('description' -> 'Invalid and undefined code').
				   ('tokens'
				    -> #( 'invalid' 'undefinedSelector' 'undefinedIdentifier' )) }
				   asDictionary).
			  ('syntax' -> {
				   ('name' -> 'syntax').
				   ('label' -> 'Syntax').
				   ('description' -> 'Any other syntactic element').
				   ('tokens'
				    ->
					    #( 'return' 'blockArgColon' 'parenthesis' 'parenthesis1'
					       'parenthesis2' 'parenthesis3' 'parenthesis4' 'parenthesis5'
					       'parenthesis6' 'parenthesis7' 'block' 'block1' 'block2' 'block3'
					       'block4' 'block5' 'block6' 'block7' 'byteArrayStart'
					       'byteArrayEnd' 'byteArrayStart1' 'byteArrayEnd1'
					       'brace' 'brace1' 'brace2' 'brace3' 'brace4' 'brace5' 'brace6'
					       'brace7' 'cascadeSeparator' 'statementSeparator'
					       'methodTempBar' 'blockTempBar' 'blockArgsBar' )) }
				   asDictionary) }
]

{ #category : 'accessing' }
SHCustomStyle class >> groups [

	^ groups ifNil: [
			  self initialize.
			  groups ]
]

{ #category : 'class initialization' }
SHCustomStyle class >> initialize [

	groups := self groupDefaults.
	groups keysAndValuesDo: [ :key :entry | | styleElement |
			styleElement := SHStyleElement new
					color: self defaultColor ;
					emphasis: self defaultEmphasis ;
					yourself.
			entry at: 'style' put: styleElement.
			styleElement when: #changed send: #styleElementChanged to: self ].
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> instanceVarStyle [

	^ self styleForGroup: 'instanceVar'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> instanceVarStyle: aStyle [ 

	^ self style: aStyle forGroup: 'instanceVar'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> messageSendsStyle [

	^ self styleForGroup: 'messageSends'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> messageSendsStyle: aStyle [ 

	^ self style: aStyle forGroup: 'messageSends'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> primitiveTypesStyle [

	^ self styleForGroup: 'primitiveTypes'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> primitiveTypesStyle: aStyle [ 

	^ self style: aStyle forGroup: 'primitiveTypes'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> reservedWordsStyle [

	^ self styleForGroup: 'reservedWords'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> reservedWordsStyle: aStyle [ 

	^ self style: aStyle forGroup: 'reservedWords'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> selectorPatternsStyle [

	^ self styleForGroup: 'selectorPatterns'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> selectorPatternsStyle: aStyle [ 

	^ self style: aStyle forGroup: 'selectorPatterns'
]

{ #category : 'settings' }
SHCustomStyle class >> settingsOn: aBuilder [

	<systemsettings>
	(aBuilder group: 'Edit Custom Style')
		parentName: 'Syntax Highlighting';
		description: 'Change the Custom style to your liking.  You have to activate it for your changes to take effect.';
		with: [
				(aBuilder button: #copyFromCurrentStyle)
					order: 0;
					description: 'Initialize custom style to colors of the currently selected style';
					target: self;
					label: 'Copy from current style';
					buttonLabel: 'Copy'.
				self groups keysAndValuesDo: [ :group :info |
						(aBuilder setting: group , 'Style')
							target: self;
							label: (info at: 'label');
							description: (info at: 'description' ifAbsent: [ '' ]) ] ]
]

{ #category : 'accessing' }
SHCustomStyle class >> style: aStyle forGroup: groupName [

	"We don't want to replace the old style element, only set it's attributes"
	"Otherwise the settings browser UI might reference an outdated StyleElement"
	(self styleForGroup: groupName)
		color: aStyle color ;
		emphasis: aStyle emphasis
]

{ #category : 'updating' }
SHCustomStyle class >> styleElementChanged [

	self triggerEvent: #changed
]

{ #category : 'accessing' }
SHCustomStyle class >> styleForGroup: groupName [

	^ (groups at: groupName) at: 'style'
]

{ #category : 'accessing' }
SHCustomStyle class >> styleTable [

	^ groups flatCollect: [ :entry |
			  (entry at: 'tokens') collect: [ :token |
					  | emphasis color |
					  emphasis := (entry at: 'style') emphasis.
					  emphasis := (emphasis includes: Character space)
						              ifTrue: [ (emphasis substrings: ' ') collect: [ :str | str asSymbol ] ]
						              ifFalse: [ emphasis asSymbol ].
						color := (entry at: 'style') color.
						Color transparent = color ifTrue: [ 
							color := nil ].
					  { token. color. emphasis } ] ]
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> syntaxStyle [

	^ self styleForGroup: 'syntax'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> syntaxStyle: aStyle [ 

	^ self style: aStyle forGroup: 'syntax'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> variableStyle [

	^ self styleForGroup: 'variable'
]

{ #category : 'settings - accessing' }
SHCustomStyle class >> variableStyle: aStyle [ 

	^ self style: aStyle forGroup: 'variable'
]
