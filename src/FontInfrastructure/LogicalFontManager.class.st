"
I reference all font families available in the  system.

For example:

LogicalFontManager current allFamilies.

FreeTypeFontSelectorDialogWindow new open.

"
Class {
	#name : #LogicalFontManager,
	#superclass : #Object,
	#instVars : [
		'fontProviders'
	],
	#pools : [
		'TextConstants'
	],
	#classInstVars : [
		'current'
	],
	#category : #FontInfrastructure
}

{ #category : #accessing }
LogicalFontManager class >> current [
	"
	current := nil.
	self current
	"
	^current ifNil:[current := self defaultCurrent]
]

{ #category : #'instance creation' }
LogicalFontManager class >> defaultCurrent [
	^self new
		addFontProvider: FreeTypeFontProvider current;
		yourself
]

{ #category : #accessing }
LogicalFontManager class >> unload [
	current ifNotNil: [current initialize].
	current := nil.

]

{ #category : #accessing }
LogicalFontManager >> addFontProvider: aFontProvider [

	fontProviders addLast: aFontProvider
	
]

{ #category : #'font families' }
LogicalFontManager >> allFamilies [
	"answer an Array containing all the font families from the receiver's fontProviders,
	together with any TextStyle font families, sorted by family name"

	| answer textStyleFamilies |
	answer := Set new.
	fontProviders do: [ :each | answer addAll: each families ].
	textStyleFamilies := TextStyle knownTextStylesWithoutDefault
		collect: [ :textStyleName | 
			TextStyleAsFontFamily new
				textStyle: (TextStyle named: textStyleName);
				familyName: textStyleName;
				yourself ].	"reject any textStyles whose defaultFont also appears as a fontFamily"
	textStyleFamilies := textStyleFamilies
		reject: [ :textStyleFamily | 
			| textStyleFamilyName |
			textStyleFamilyName := textStyleFamily textStyle defaultFont familyName.
			answer anySatisfy: [ :fontFamily | fontFamily familyName = textStyleFamilyName ] ].
	answer addAll: textStyleFamilies.
	^ (answer asSortedCollection: [ :a :b | a familyName <= b familyName ]) asArray
]

{ #category : #'font lookup' }
LogicalFontManager >> bestFontFor: aLogicalFont [
	"look up best font from the receivers fontProviders"
	
	^self bestFontFor: aLogicalFont whenFindingAlternativeIgnoreAll: Set new

]

{ #category : #'font lookup' }
LogicalFontManager >> bestFontFor: aLogicalFont whenFindingAlternativeIgnoreAll: ignoreSet [
	"look up best real font from the receivers fontProviders.
	If we can't find a font, then answer an alternative real font.
	
	ignoreSet contains the LogicalFonts that we have already attempted to
	get an alternative real font from. We ignore those on each iteration so that we don't 
	recurse forever"
	| textStyle font |
	
	aLogicalFont familyNames do:[:familyName |
		fontProviders do:[:p | | answer |
			(answer := p fontFor: aLogicalFont familyName: familyName)
				ifNotNil:[^answer]].
		textStyle := TextStyle named: familyName.
		textStyle ifNotNil:[
			font := textStyle fontOfPointSize: aLogicalFont pointSize.
			font ifNotNil:[^font emphasized: aLogicalFont emphasis]]].
	"not found, so use the default TextStyle"
	textStyle := TextStyle default.
	textStyle ifNotNil:[
		font := textStyle fontOfPointSize: aLogicalFont pointSize.
		(font isKindOf: LogicalFont) ifFalse:[^font emphasized: aLogicalFont emphasis].
		(ignoreSet includes: font)
			ifFalse:[
				ignoreSet add: font.  "remember that we have visited font so that we don't loop forever"
				"try again using the default TextStyle's logicalFont"
				^self bestFontFor: font whenFindingAlternativeIgnoreAll: ignoreSet]].
	"Neither the family, nor any of the fallback families, is available. 
	Any non-LogicalFont will do as a fallback"
	(TextSharedInformation select: [:each | each isKindOf: TextStyle]) 
		do:[:ts | 
			((font := ts fontOfPointSize: aLogicalFont pointSize) isKindOf: LogicalFont)
				ifFalse:[^font emphasized:  aLogicalFont emphasis]].
	"There are no non-logical fonts in TextSharedInformation - let it fail by answering nil"
	^nil
	
			

]

{ #category : #initialization }
LogicalFontManager >> initialize [
	super initialize.
	fontProviders := OrderedCollection new: 10
	
]
