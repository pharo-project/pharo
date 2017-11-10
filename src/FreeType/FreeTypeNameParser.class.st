"
A name parser
"
Class {
	#name : #FreeTypeNameParser,
	#superclass : #Object,
	#instVars : [
		'combinedName',
		'familyNameIn',
		'styleNameIn',
		'delimiters',
		'tokens',
		'extractedSlant',
		'extractedSlantValue',
		'extractedUpright',
		'extractedStretch',
		'extractedWeight',
		'italicFlag',
		'boldFlag',
		'extractedWeightValue',
		'extractedStretchValue'
	],
	#classInstVars : [
		'weightNames',
		'stretchNames',
		'obliqueNames',
		'normalNames',
		'italicNames'
	],
	#category : #FreeType-FontManager
}

{ #category : #'known names' }
FreeTypeNameParser class >> italicAndObliqueNames [
	^self italicNames, self obliqueNames
]

{ #category : #'known names' }
FreeTypeNameParser class >> italicNames [
	"Answer a sequence of String tokens that indicate an italic font
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #italicNames put: nil.
	"
	italicNames ifNotNil:[^italicNames].
	^italicNames := #(
		'ita'
		'ital'
		'italic'
		'cursive'
		'kursiv').
]

{ #category : #'known names' }
FreeTypeNameParser class >> normalNames [
	"Answer a sequence of String tokens that indicate a Regular
	(i.e. non-oblique, non-italic) font within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #normalNames put: nil.
	"
	normalNames ifNotNil:[^normalNames].
	^normalNames := #('Book' 'Normal' 'Regular' 'Roman' 'Upright').
]

{ #category : #'known names' }
FreeTypeNameParser class >> obliqueNames [
	"Answer a sequence of String tokens that indicate an oblique font
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #obliqueNames put: nil.
	"
	obliqueNames ifNotNil:[^obliqueNames].
	^obliqueNames := #(
		'inclined'
		'oblique'
		'backslanted'
		'backslant'
		'slanted').
]

{ #category : #'known names' }
FreeTypeNameParser class >> stretchNames [
	"Answer a sequence of arrays.
	Each array has an integer stretch value as its first element (1 - 9).
	The remaining elements are String tokens which might appear
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #stretchNames put: nil.
	"
	stretchNames ifNotNil:[^stretchNames].
	^stretchNames := {
		{LogicalFont stretchExtraCompressed. 'extra'. 'compressed'}.
		{LogicalFont stretchExtraCompressed. 'extracompressed'}.
		{LogicalFont stretchExtraCompressed. 'ext'. 'compressed'}.
		{LogicalFont stretchExtraCompressed. 'extcompressed'}.
		{LogicalFont stretchUltraCompressed. 'ultra'. 'compressed'}.
		{LogicalFont stretchUltraCompressed. 'ultracompressed'}.
		{LogicalFont stretchUltraCondensed. 'ultra'. 'condensed'}.
		{LogicalFont stretchUltraCondensed. 'ultracondensed'}.
		{LogicalFont stretchUltraCondensed. 'ultra'. 'cond'}.
		{LogicalFont stretchUltraCondensed. 'ultracond'}.
		{LogicalFont stretchCompressed. 'compressed'}.
		{LogicalFont stretchExtraCondensed. 'extra'. 'condensed'}.
		{LogicalFont stretchExtraCondensed. 'extracondensed'}.
		{LogicalFont stretchExtraCondensed. 'ext'. 'condensed'}.
		{LogicalFont stretchExtraCondensed. 'extcondensed'}.
		{LogicalFont stretchExtraCondensed. 'extra'. 'cond'}.
		{LogicalFont stretchExtraCondensed. 'extracond'}.
		{LogicalFont stretchExtraCondensed. 'ext'. 'cond'}.
		{LogicalFont stretchExtraCondensed. 'extcond'}.
		{LogicalFont stretchNarrow. 'narrow'}.
		{LogicalFont stretchCompact. 'compact'}.
		{LogicalFont stretchSemiCondensed. 'semi'. 'condensed'}.
		{LogicalFont stretchSemiCondensed. 'semicondensed'}.
		{LogicalFont stretchSemiCondensed. 'semi'. 'cond'}.
		{LogicalFont stretchSemiCondensed. 'semicond'}.
		{LogicalFont stretchWide. 'wide'}.
		{LogicalFont stretchSemiExpanded. 'semi'. 'expanded'}.
		{LogicalFont stretchSemiExpanded. 'semiexpanded'}.
		{LogicalFont stretchSemiExtended. 'semi'. 'extended'}.
		{LogicalFont stretchSemiExtended. 'semiextended'}.
		{LogicalFont stretchExtraExpanded. 'extra'. 'expanded'}.
		{LogicalFont stretchExtraExpanded. 'extraexpanded'}.
		{LogicalFont stretchExtraExpanded. 'ext'. 'expanded'}.
		{LogicalFont stretchExtraExpanded. 'extexpanded'}.
		{LogicalFont stretchExtraExtended. 'extra'. 'extended'}.
		{LogicalFont stretchExtraExtended. 'extraextended'}.
		{LogicalFont stretchExtraExtended. 'ext'. 'extended'}.
		{LogicalFont stretchExtraExtended. 'extextended'}.
		{LogicalFont stretchUltraExpanded. 'ultra'. 'expanded'}.
		{LogicalFont stretchUltraExpanded. 'ultraexpanded'}.
		{LogicalFont stretchUltraExtended. 'ultra'. 'extended'}.
		{LogicalFont stretchUltraExtended. 'ultraextended'}.
		{LogicalFont stretchCondensed. 'condensed'}.
		{LogicalFont stretchCondensed. 'cond'}.
		{LogicalFont stretchExpanded. 'expanded'}.
		{LogicalFont stretchExtended. 'extended'}	
		}.  "search for them in the order given here"

]

{ #category : #'known names' }
FreeTypeNameParser class >> weightNames [
	"Answer a sequence of arrays.
	Each array has an integer weight value as its first element.
	The remaining elements are String tokens which might appear
	within a font family-style name"
	"
	TO RE-INITIALIZE...
	self instVarNamed: #weightNames put: nil.
	"
	weightNames ifNotNil:[^weightNames].
	^weightNames := {
		{LogicalFont weightExtraThin. 'extra'. 'thin'}.
		{LogicalFont weightExtraThin.'extrathin'}.
		{LogicalFont weightExtraThin. 'ext'. 'thin'}.
		{LogicalFont weightExtraThin. 'extthin'}.
		{LogicalFont weightUltraThin.'ultra'. 'thin'}.
		{LogicalFont weightUltraThin.'ultrathin'}.
		{LogicalFont weightExtraLight. 'extra'. 'light'}.
		{LogicalFont weightExtraLight. 'extralight'}.
		{LogicalFont weightExtraLight. 'ext'. 'light'}.
		{LogicalFont weightExtraLight. 'extlight'}.
		{LogicalFont weightUltraLight. 'ultra'. 'light'}.
		{LogicalFont weightUltraLight. 'ultralight'}.
		{LogicalFont weightSemiBold. 'semi'. 'bold'}.
		{LogicalFont weightSemiBold. 'semibold'}.
		{LogicalFont weightDemiBold. 'demi'. 'bold'}.
		{LogicalFont weightDemiBold. 'demibold'}.
		{LogicalFont weightExtraBold. 'extra'. 'bold'}.
		{LogicalFont weightExtraBold. 'extrabold'}.
		{LogicalFont weightExtraBold. 'ext'. 'bold'}.
		{LogicalFont weightExtraBold. 'extbold'}. 
		{LogicalFont weightUltraBold. 'ultra'. 'bold'}.
		{LogicalFont weightUltraBold. 'ultrabold'}.
		{LogicalFont weightExtraBlack. 'extra'. 'black'}.
		{LogicalFont weightExtraBlack. 'extrablack'}.
		{LogicalFont weightExtraBlack. 'ext'. 'black'}.
		{LogicalFont weightExtraBlack. 'extblack'}.
		{LogicalFont weightUltraBlack.'ultra'. 'black'}.
		{LogicalFont weightUltraBlack. 'ultrablack'}.
		{LogicalFont weightBold. 'bold'}.
		{LogicalFont weightThin.'thin'}.
		{LogicalFont weightLight. 'light'}.
		{LogicalFont weightMedium. 'medium'}.
		{LogicalFont weightBlack. 'black'}.
		{LogicalFont weightHeavy. 'heavy'}.
		{LogicalFont weightNord. 'nord'}.
		{LogicalFont weightDemi. 'demi'}.
		{LogicalFont weightUltra. 'ultra'}.
		} 
			
]

{ #category : #parsing }
FreeTypeNameParser >> addStyleNameToCombinedName: aStyleString [
	| lcCombined lcStyleName addStyle index |
	lcCombined := combinedName asLowercase.
	lcStyleName := aStyleString asLowercase.
	addStyle := true.
	(index := lcCombined findString: lcStyleName) > 0
		ifTrue:[ 
			(index = 1 or:[delimiters includes: (lcCombined at: index - 1)])
				ifTrue:[
					((index + lcStyleName size > lcCombined size) or:[ delimiters includes: (lcCombined at: index + lcStyleName size) ])
						ifTrue:["don't add the style to the combinedName, because it already contains it"
							addStyle := false]]].
	addStyle ifTrue:[combinedName := combinedName , ' ', aStyleString].
	
]

{ #category : #accessing }
FreeTypeNameParser >> boldFlag: aBoolean [
	boldFlag := aBoolean
]

{ #category : #parsing }
FreeTypeNameParser >> extractSlant [

	|  matches start end |

	"match and remove last italic/oblique token"
	extractedSlant := nil.
	extractedSlantValue := LogicalFont slantRegular. "not italic or oblique"
	(self italicAndObliqueNames 
		detect: [:each |
			(matches := self lastMatchValueSequence: {each}) notNil]
		ifNone:[]) ifNotNil:[
			start := matches first second.
			end :=  matches last third.
			extractedSlant := combinedName copyFrom: start to: end.
			"extractedSlantValue := (self italicNames includes: extractedSlant asLowercase)
				ifTrue:[1]
				ifFalse:[2]."
			extractedSlantValue := LogicalFont slantItalic. "treat italic and oblique the same, as italic"
			[start > 1 and:[delimiters includes: (combinedName at: start - 1)]] "also remove delimiters before token"
				whileTrue:[start := start - 1].
			[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]] "also remove delimiters after token"
				whileTrue:[end := end + 1].		
			combinedName := combinedName copyReplaceFrom: start to: end with: ' '.].
	(extractedSlant isNil and:[italicFlag])
		ifTrue:["no italic specified in familyName or styleName; force it to be 'Italic'"
			extractedSlant := 'Italic'.
			extractedSlantValue := LogicalFont slantItalic]		
]

{ #category : #parsing }
FreeTypeNameParser >> extractStretch [
	"match and remove last stretch tokens"
	| matches start end |

	extractedStretchValue := LogicalFont stretchRegular.
	(self stretchNames 
		detect: [:each |
			matches := self lastMatchValueSequence: each allButFirst.
			matches ifNotNil:[extractedStretchValue := each first].
			matches notNil]
		ifNone:[]) ifNotNil:[
			start := matches first second.
			end :=  matches last third.
			extractedStretch := combinedName copyFrom: start to: end.
			[start > 1 and:[delimiters includes: (combinedName at: start - 1)]] "also remove delimiters before token"
				whileTrue:[start := start - 1].
			[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]] "also remove delimiters after token"
				whileTrue:[end := end + 1].		
			combinedName := combinedName copyReplaceFrom: start to: end with: ' '.
			"re-tokenize"
			"tokens := self tokenize: combinedName delimiters: delimiters"].	
]

{ #category : #parsing }
FreeTypeNameParser >> extractUpright [
	"extract from current combined name.
	answer new combinedName"
	| normalTok start end | 

	normalTok := tokens reversed 
		detect: [:tok | 
			(self normalNames 
				detect: [:str | str asLowercase = tok first asLowercase] 
				ifNone:[]) notNil ]
		ifNone:[].
	normalTok ifNotNil:[
		"remove it from combinedName"
		start := normalTok second.
		end :=  normalTok third.
		extractedUpright := combinedName copyFrom: start to: end.
		[start > 1 and:[delimiters includes: (combinedName at: start - 1)]]
			whileTrue:[start := start - 1].
		[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]]
			whileTrue:[end := end + 1].		
		combinedName := combinedName copyReplaceFrom: start to: end with: ' '].

	
]

{ #category : #parsing }
FreeTypeNameParser >> extractWeight [
	"match and remove last weight tokens"
	| matches start end |

	extractedWeightValue := LogicalFont weightRegular.
	(self weightNames 
		detect: [:each |
			matches := self lastMatchValueSequence: each allButFirst.
			matches ifNotNil:[extractedWeightValue := each first].
			matches notNil]
		ifNone:[]) ifNotNil:[
			start := matches first second.
			end :=  matches last third.
			extractedWeight := combinedName copyFrom: start to: end.
			[start > 1 and:[delimiters includes: (combinedName at: start - 1)]] "also remove delimiters before token"
				whileTrue:[start := start - 1].
			[end < combinedName size and:[delimiters includes: (combinedName at: end + 1)]] "also remove delimiters after token"
				whileTrue:[end := end + 1].		
			combinedName := combinedName copyReplaceFrom: start to: end with: ' '.].	
	(extractedWeight isNil and:[boldFlag])
		ifTrue:["no weight specified in familyName or styleName; force it to be 'Bold'"
			extractedWeight := 'Bold'.
			extractedWeightValue := LogicalFont weightBold]	
]

{ #category : #accessing }
FreeTypeNameParser >> extractedSlant [
	^extractedSlant
]

{ #category : #accessing }
FreeTypeNameParser >> extractedSlantValue [
	^extractedSlantValue
]

{ #category : #accessing }
FreeTypeNameParser >> extractedStretch [
	^extractedStretch
]

{ #category : #accessing }
FreeTypeNameParser >> extractedStretchValue [
	^extractedStretchValue
]

{ #category : #accessing }
FreeTypeNameParser >> extractedUpright [
	^extractedUpright
]

{ #category : #accessing }
FreeTypeNameParser >> extractedWeight [
	^extractedWeight
]

{ #category : #accessing }
FreeTypeNameParser >> extractedWeightValue [
	^extractedWeightValue
]

{ #category : #accessing }
FreeTypeNameParser >> familyName [
	^combinedName trimBoth
]

{ #category : #accessing }
FreeTypeNameParser >> familyName: familyName [
	familyNameIn := familyName.

	
]

{ #category : #accessing }
FreeTypeNameParser >> familyNameIn: familyName [
	familyNameIn := familyName.

	
]

{ #category : #initialization }
FreeTypeNameParser >> initialize [
	super initialize.
	delimiters := ',.-_'.
	Character separators do:[:c | delimiters := delimiters , c asString].

]

{ #category : #'known names' }
FreeTypeNameParser >> italicAndObliqueNames [
	^self class italicAndObliqueNames
]

{ #category : #accessing }
FreeTypeNameParser >> italicFlag: aBoolean [
	italicFlag := aBoolean
]

{ #category : #'known names' }
FreeTypeNameParser >> italicNames [
	^self class italicNames
]

{ #category : #parsing }
FreeTypeNameParser >> lastMatchValueSequence: values [
	"answer the last contiguous tokens that match pattern tokens, 
	or nil if not found.
	matching is case insensitive "
	| answer  nullToken match tok |
	nullToken := {''. nil. nil}.
	tokens size - values size + 1 to: 1 by: -1 do:[:ti |
		match := true.
		answer := Array new.
		1 to: values size do:[:vi |
			tok := tokens at: ti + vi - 1 ifAbsent: [nullToken].
			(match and: [tok first asLowercase = ( values at: vi) asLowercase])
				ifFalse:[match := false]
				ifTrue:[answer := answer, {tok} ]].
		match ifTrue:[^answer]].
	^nil
			

]

{ #category : #'known names' }
FreeTypeNameParser >> normalNames [
	^self class normalNames

]

{ #category : #parsing }
FreeTypeNameParser >> parse [

	| styleName |
	styleNameIn := self splitBadTokensIn: styleNameIn.
	combinedName := styleNameIn trimBoth.
	tokens := self tokenize: combinedName.
	self extractUpright.
	styleName := combinedName.
	combinedName := familyNameIn trimBoth.
	self addStyleNameToCombinedName: styleName..
	tokens := self tokenize: combinedName.
	self extractSlant.
	tokens := self tokenize: combinedName.
	self extractStretch.
	tokens := self tokenize: combinedName.
	self extractWeight.
	
]

{ #category : #parsing }
FreeTypeNameParser >> splitBadTokensIn: aString [
	"split tokens such as BoldOblique, that should be two words"
	| str |
	str := aString.
	#(	('bold' 'oblique') ('bold' 'italic')
	) do:[:pair | | i |
		(i := str asLowercase findString: pair first, pair second startingAt: 1) > 0
			ifTrue:[
				str := (str first: i + pair first size - 1), ' ', (str last: (str size - (i + pair first size - 1)))]].
	^str
]

{ #category : #'known names' }
FreeTypeNameParser >> stretchNames [
	^self class stretchNames
]

{ #category : #accessing }
FreeTypeNameParser >> styleName: styleName [
	
	styleNameIn := styleName. 

]

{ #category : #accessing }
FreeTypeNameParser >> styleNameIn: styleName [
	
	styleNameIn := styleName. 

]

{ #category : #parsing }
FreeTypeNameParser >> tokenize: aString [ 
	"answer an OrderedCollection of {string. start. end} tuples.
	tokens are separated by $- $_ $, $. and whitespace"
	| currentTokens answer start |
	
	currentTokens := aString findTokens: delimiters keep: delimiters.
	answer := OrderedCollection new.
	start := 1.
	currentTokens do:[:tok |
		(delimiters includes: tok first)
			ifFalse:[answer add: {tok. start. start+tok size - 1}].
		start := start + tok size].
	^answer
]

{ #category : #'known names' }
FreeTypeNameParser >> weightNames [
	^self class weightNames
			
]
