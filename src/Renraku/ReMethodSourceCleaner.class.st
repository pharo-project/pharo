"
A source code cleaner
"
Class {
	#name : #ReMethodSourceCleaner,
	#superclass : #Object,
	#instVars : [
		#sourceLines,
		#keepFinalDot
	],
	#classVars : [
		#KeepFinalDotInMethodBody
	],
	#category : #'Renraku-Utility'
}

{ #category : #cleaning }
ReMethodSourceCleaner class >> clean: aString [

	^ (self forSource: aString)
		  clean;
		  contents
]

{ #category : #cleaning }
ReMethodSourceCleaner class >> clean: aString keepFinalDot: aBoolean [

	^ (self forSource: aString)
			keepFinalDot: aBoolean;
		   clean;
		   contents
]

{ #category : #settings }
ReMethodSourceCleaner class >> compactSourceCodeCleanupSettingsOn: aBuilder [
	<systemsettings>

	(aBuilder setting: #keepFinalDotInMethodBody)
		order: 16;
		label: 'Keep final dot dot in method body when critique is autofixed';
		description: 'Define if the system keeps an unnecessary final dot at the end of a method body when critique on it is automatically resolved';
		parent: #qualityAssistant;
		default: false;
		target: self
]

{ #category : #'instance creation' }
ReMethodSourceCleaner class >> forSource: aString [

	^ self new
		initFromSource: aString;
		yourself
]

{ #category : #testing }
ReMethodSourceCleaner class >> isNecessaryToClean: aCompiledMethod allowFinalDot: aBoolean [
	"Return true if the receiver finds out that it is necessary to clean the source code of the
	 given method (which means cleaned source code would have different size than original method)"

	| source |
	source := aCompiledMethod sourceCode.
	^ (self clean: source keepFinalDot: aBoolean) size ~= aCompiledMethod sourceCode size
]

{ #category : #'settings - accessing' }
ReMethodSourceCleaner class >> keepFinalDotInMethodBody [

	^ KeepFinalDotInMethodBody ifNil: [ KeepFinalDotInMethodBody := false ]
]

{ #category : #'settings - accessing' }
ReMethodSourceCleaner class >> keepFinalDotInMethodBody: aBoolean [

	KeepFinalDotInMethodBody := aBoolean
]

{ #category : #utilities }
ReMethodSourceCleaner class >> showCleaningOf: aString [

	SpDiffPresenter new
 		showOptions: false;
		leftText: aString;
		rightText: (self clean: aString);
		open
]

{ #category : #operating }
ReMethodSourceCleaner >> clean [

	self
		removeTrailingSeparatorLines;
		removeTrailingDot;
		removeTrailingSeparatorsInAllLines
]

{ #category : #accessing }
ReMethodSourceCleaner >> contents [

	^ String streamContents: [:s |
			sourceLines asArray do: [:each | s nextPutAll: each contents ] separatedBy: [ s nextPut: Character cr ]
	]
]

{ #category : #'private - initialization' }
ReMethodSourceCleaner >> initFromSource: aString [

	| lines |
	lines := aString lines withIndexCollect: [ :each :index |
		         ReSourceCodeLine lineNumber: index contents: each ].

	sourceLines := DoubleLinkedList new
		               addAll: lines;
		               yourself
]

{ #category : #initialization }
ReMethodSourceCleaner >> initialize [

	super initialize.
	self keepFinalDot: self class keepFinalDotInMethodBody
]

{ #category : #accessing }
ReMethodSourceCleaner >> keepFinalDot [

	^ keepFinalDot
]

{ #category : #accessing }
ReMethodSourceCleaner >> keepFinalDot: aBoolean [

	keepFinalDot := aBoolean
]

{ #category : #'private - operating' }
ReMethodSourceCleaner >> removeTrailingDot [

	self keepFinalDot ifTrue: [ ^ self ].
	sourceLines last removeTrailingDot
]

{ #category : #'private - operating' }
ReMethodSourceCleaner >> removeTrailingSeparatorLines [

	[
	sourceLines isNotEmpty and: [
		sourceLines last onlyContainsSeparators ] ] whileTrue: [
		sourceLines removeLast ]
]

{ #category : #'private - operating' }
ReMethodSourceCleaner >> removeTrailingSeparatorsInAllLines [

	 sourceLines do: [:each | each removeTrailingSeparators ]
]
