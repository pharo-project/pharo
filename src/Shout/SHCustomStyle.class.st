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

{ #category : 'accessing' }
SHCustomStyle class >> color: aColor forGroup: aGroupName [

	(groups at: aGroupName) at: 'color' put: aColor.
	SHPreferences customStyleChanged
]

{ #category : 'accessing' }
SHCustomStyle class >> colorForGroup: groupName [

	^ (groups at: groupName) at: 'color'
]

{ #category : 'class initialization' }
SHCustomStyle class >> copyFromCurrentStyle [

	SHRBTextStyler styleTable do: [ :row |
			| token |
			token := row first.
			groups
				detect: [ :entry | token = (entry at: 'tokens') first ]
				ifFound: [ :entry |
						| colorDesc emphasis |
						colorDesc := row second.
						emphasis := #normal.
						row size >= 3 ifTrue: [ emphasis := row third ].
						(emphasis isKindOf: Symbol) ifFalse: [
							emphasis := (' ' join: emphasis) asSymbol ].
						entry at: 'color' put: (Color colorFrom: colorDesc).
						entry at: 'emphasis' put: emphasis ] ].
	SettingBrowser refreshAllSettingBrowsers
]

{ #category : 'private' }
SHCustomStyle class >> defaultColor [

	^ Color r: 0.0 g: 0.75 b: 1.0
]

{ #category : 'private' }
SHCustomStyle class >> defaultEmphasis [

	^ #normal
]

{ #category : 'accessing' }
SHCustomStyle class >> emphasis: anEmphasis forGroup: aGroupName [

	(groups at: aGroupName) at: 'emphasis' put: anEmphasis.
	SHPreferences customStyleChanged
]

{ #category : 'accessing' }
SHCustomStyle class >> emphasisForGroup: groupName [

	^ (groups at: groupName) at: 'emphasis'
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
	groups do: [ :entry |
			entry at: 'color' put: self defaultColor.
			entry at: 'emphasis' put: self defaultEmphasis ].
]

{ #category : 'accessing' }
SHCustomStyle class >> styleTable [

	^ groups flatCollect: [ :entry |
			  (entry at: 'tokens') collect: [ :token |
					  | emphasis |
					  emphasis := entry at: 'emphasis'.
					  (emphasis includes: Character space) ifTrue: [
						  emphasis := (emphasis substrings: ' ') collect: [ :str |
							              str asSymbol ] ].
					  {
						  token.
						  (entry at: 'color').
						  emphasis } ] ]
]
