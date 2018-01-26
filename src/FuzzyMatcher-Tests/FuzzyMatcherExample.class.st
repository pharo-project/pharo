"
FuzzyExample is a class used to showcase the FuzzyMatcher score matching. 
See the example on the class side.

"
Class {
	#name : #FuzzyMatcherExample,
	#superclass : #Object,
	#instVars : [
		'textModel',
		'dataSource',
		'elements',
		'matchedElements',
		'pattern',
		'stringMorph'
	],
	#category : #'FuzzyMatcher-Tests'
}

{ #category : #'as yet unclassified' }
FuzzyMatcherExample class >> exampleAuthors [
	<sampleInstance>
	| authors blk |
	
	authors := OrderedCollection new.
	blk := [ :mtd | 
		mtd timeStamp ifNotEmpty: [ :stamp |
			(' ' join: ((stamp splitOn: ' ') allButLast: 2)) ifNotEmpty: [ :author |
				 author trimmed ifNotEmpty: [ authors add: author ]
			]
		] 
	].
	
	Smalltalk allClassesAndTraitsDo: [ :each |
		each methodsDo: blk. 
		each theMetaClass methodsDo: blk.
	].

	^ self new 
		elements: authors asSet asArray;
		open
]

{ #category : #examples }
FuzzyMatcherExample class >> exampleClasses [
	<sampleInstance>

	^ self new 
		pattern: 'mph';
		elements: (Smalltalk allClassesAndTraits collect: #name);
		open
]

{ #category : #examples }
FuzzyMatcherExample class >> examplePackages [
	<sampleInstance>

	^ self new 
		pattern: '-st';
		elements: (RPackage organizer packages collect: #name);
		open
]

{ #category : #examples }
FuzzyMatcherExample class >> exampleSelectors [
	<sampleInstance>

	^ self new 
		pattern: '::::::::::';
		elements: Symbol selectorTable asArray;
		open 
]

{ #category : #private }
FuzzyMatcherExample >> aquireFont [

	^ LogicalFont familyName: self fontName pointSize: 16
]

{ #category : #opening }
FuzzyMatcherExample >> buildMorph [

	| morph ft |
	
	morph := AlignmentMorph newColumn
		color: Color transparent;  
		cellInset: 4@4;
		yourself.
	
	textModel := textModel newTextField
		textFont: self aquireFont;
		hResizing: #spaceFill;
		yourself.
		
	ft := FTTableMorph new
		dataSource: dataSource;
		vResizing: #spaceFill; 
		hResizing: #spaceFill;
		yourself.

	morph 
		addMorph: ft;
		addMorph: stringMorph;
		addMorph: textModel.
	
	^ morph
	
]

{ #category : #accessing }
FuzzyMatcherExample >> elements: aCollection [

	elements := aCollection collect: [ :each | each -> 0 ].
	
	matchedElements := elements.
	
	dataSource elements: matchedElements.
	
	self update
	
]

{ #category : #private }
FuzzyMatcherExample >> fontName [

	^ 'Source Sans Pro'
]

{ #category : #initialization }
FuzzyMatcherExample >> initialize [

	super initialize.
	
	pattern := ''.
	
	dataSource := FTEasyListDataSource new
		elements: #();
		display: [ :item |
			String streamContents: [ :stream |			
				stream 
					nextPutAll: item value asString;
					space: (10 - ((stream position *2) min: 8));
					nextPutAll: item key asString
			]
		];
		yourself.
				
	textModel := RubScrolledTextModel new.
	textModel announcer when: RubEditsStateChangedInModel 
		send: #update to: self.
	
	stringMorph := StringMorph contents: '' font: self aquireFont
		
	
]

{ #category : #private }
FuzzyMatcherExample >> match: aPattern for: aCollection [

	| matcher matches |
	
	aPattern isEmpty ifTrue: [ ^ aCollection ].
	
	matcher := FuzzyMatcher pattern: aPattern.

	matches := OrderedCollection new: aCollection size.
	
	aCollection do: [ :each | 
		matcher 
			match: each key
			ifScored: [ :score | matches add: each key -> score ] 
	].
	
	matches sort: [ :a :b | a value >= b value ].
	
	^ matches
]

{ #category : #opening }
FuzzyMatcherExample >> open [

	| window area |
	
	window := (self buildMorph openInWindowLabeled: 'Fuzzy Matching Example').
	area := window world clearArea.
	
	window 
		extent: (area extent * (0.6 @ 0.7)) asIntegerPoint;
		center: area center.
	
	^ window
]

{ #category : #accessing }
FuzzyMatcherExample >> pattern: aString [

	textModel setText: aString.
]

{ #category : #private }
FuzzyMatcherExample >> setElements: aCollection [

	elements := aCollection collect: [ :each | each -> 0 ].
	
	matchedElements := elements.
	
	dataSource elements: matchedElements.
]

{ #category : #'event handling' }
FuzzyMatcherExample >> update [

	self updatePattern: textModel getText asString
]

{ #category : #'event handling' }
FuzzyMatcherExample >> updatePattern: newPattern [

	| searchPool t text |

	pattern = newPattern ifTrue: [ ^ self ].

	searchPool := pattern size > newPattern size 
		ifTrue: [ elements ]
		ifFalse: [ matchedElements ].

	t := [ matchedElements := self match: newPattern for: searchPool ] timeToRun.
	
	text := 'Checked {1} items, found {2} matching | {3} ms' format: { 
		elements size . matchedElements size. t asMilliSeconds }.
	
	stringMorph contents: text.		
	pattern := newPattern.	
	dataSource elements: matchedElements.	
	dataSource table ifNotNil: [ :fastTable | fastTable refresh ].
	
]
