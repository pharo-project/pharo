"
FontSet provides a mechanism for storing a set of fonts as a class that can be conveniently filedOut, filedIn, and installed as a TextStyle.

The most common use is...
	Find a font you like.
	Use BitFont to convert a bunch of sizes to data files named, eg, LovelyNN.BF
	Use FontSet convertFontsNamed: 'Lovely' to produce a FontSet named Lovely.
	FileOut that FontSet for later use.
	Use Lovely installAsTextStyle to make all sizes available in a TextStyle
		named #Lovely in the TextConstants dictionary.
	Use ctrl-k in any text pane to select the new Lovely style for that paragraph.
	Then use cmd-1 through 5 or cmd-k to set the point-size for any selection.

"
Class {
	#name : #FontSet,
	#superclass : #Object,
	#pools : [
		'TextConstants'
	],
	#category : #'Graphics-Fonts'
}

{ #category : #compiling }
FontSet class >> acceptsLoggingOfCompilation [
	"Dont log sources for my subclasses, so as not to waste time
	and space storing printString versions of the string literals."

	^ self == FontSet
]

{ #category : #'filein/out' }
FontSet class >> fileOut [
	"FileOut and then change the properties of the file so that it won't be
	treated as text by, eg, email attachment facilities"

	super fileOut.
	(FileStream oldFileNamed: self name , '.st') close
]

{ #category : #private }
FontSet class >> fontCategory [
	^ 'Graphics-Fonts' asSymbol
]

{ #category : #private }
FontSet class >> fontName [
	^ self name asSymbol
]

{ #category : #installing }
FontSet class >> fontNamed: fontName fromMimeLiteral: aString [
	"This method allows a font set to be captured as sourcecode in a subclass.
	The string literals will presumably be created by printing, eg,
		(FileStream readOnlyFileNamed: 'Palatino24.sf2') contentsOfEntireFile,
		and following the logic in compileFont: to encode and add a heading.

	See the method installAsTextStyle to see how this can be used."

	^ StrikeFont new 
		name: fontName;
		readFromStrike2Stream: aString base64Decoded readStream
]

{ #category : #private }
FontSet class >> fontSetClass: aString [ 
	| className fontSet |
	className := (self name , (aString select: [ :c | c isAlphaNumeric ]) capitalized) asSymbol.
	fontSet := Smalltalk globals
		at: className
		ifAbsentPut: 
			[ self 
				subclass: className
				instanceVariableNames: ''
				classVariableNames: ''
				poolDictionaries: ''
				category: self fontCategory ].
	(fontSet inheritsFrom: self) ifFalse: [ ^ self error: 'The name ' , className , ' is already in use' ].
	^ fontSet
]

{ #category : #installing }
FontSet class >> installAsDefault [ 
	"FontSetNewYork installAsDefault"
	
	(self confirm: 'Do you want to install' translated, '
''' , self fontName , ''' as default font?' translated)
		ifFalse: [^ self].
	self installAsTextStyle.
	TextSharedInformation at: #DefaultTextStyle put: (TextStyle named: self fontName).
	

]

{ #category : #installing }
FontSet class >> installAsTextStyle [
	"FontSetNewYork installAsTextStyle"
	| selectors |
	(TextSharedInformation includesKey: self fontName) ifTrue: 
		[ (self confirm: self fontName , ' is already defined in TextSharedInformation.
Do you want to replace that definition?') ifFalse: [ ^ self ] ].
	selectors := (self class selectors select: [ :s | s beginsWith: 'size' ]) asSortedCollection.
	TextSharedInformation 
		at: self fontName
		put: (TextStyle fontArray: (selectors collect: [ :each | self perform: each ]))
]
