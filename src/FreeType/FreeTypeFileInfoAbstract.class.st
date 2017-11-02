"
Abstract superclass for file infos for a FreeType font
"
Class {
	#name : #FreeTypeFileInfoAbstract,
	#superclass : #Object,
	#instVars : [
		'index',
		'familyName',
		'styleName',
		'postscriptName',
		'bold',
		'italic',
		'fixedWidth',
		'numFaces',
		'familyGroupName',
		'slant',
		'slantValue',
		'weight',
		'stretch',
		'weightValue',
		'stretchValue',
		'styleNameExtracted',
		'upright'
	],
	#category : 'FreeType-FontManager'
}

{ #category : #accessing }
FreeTypeFileInfoAbstract >> bold [
	"Answer the value of bold"

	^ bold
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> bold: anObject [
	"Set the value of bold"

	bold := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> extractAttributesFromNames [
	"derive values for the receiver's style(italic), weight, and stretch inst vars.
	Also set the familyGroupName and styleNameExtracted"

	| p |

	p:= FreeTypeNameParser new
		familyNameIn: self validFamilyName;
		styleNameIn: self validStyleName;
		italicFlag: italic;
		boldFlag: bold;
		parse.
	familyGroupName := p familyName.
	slant := p extractedSlant.
	slantValue := p extractedSlantValue.
	weight := p extractedWeight.
	weightValue := p extractedWeightValue.
	stretch := p extractedStretch.
	stretchValue := p extractedStretchValue.
	upright := p extractedUpright.
	styleNameExtracted := ''.
	stretch ifNotNil:[
		styleNameExtracted := styleNameExtracted ,stretch].
	(weight notNil "and:[weight asLowercase ~= 'medium']") 
		ifTrue:[
			styleNameExtracted := styleNameExtracted , ' ', weight].
	slant ifNotNil:[
		styleNameExtracted := styleNameExtracted , ' ', slant].
	styleNameExtracted := styleNameExtracted trimBoth.
	styleNameExtracted ifEmpty: [
		styleNameExtracted := upright ifNil:['Regular']].
		 
	
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> familyGroupName [
	"Answer the value of familyGroupName"

	^ familyGroupName
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> familyName [
	"Answer the value of familyName"

	^ familyName
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> familyName: anObject [
	"Set the value of familyName"

	familyName := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> fixedWidth [
	"Answer the value of fixedWidth"

	^ fixedWidth
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> fixedWidth: anObject [
	"Set the value of fixedWidth"

	fixedWidth := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> index [
	"Answer the value of index"

	^ index
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> index: anObject [
	"Set the value of index"

	index := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> isBolderThan: val [
	^self weightValue >= val
]

{ #category : #testing }
FreeTypeFileInfoAbstract >> isEmbedded [
	^false
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> isItalicOrOblique [
	^self slantValue > 0
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> italic [
	"Answer the value of italic"

	^ italic
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> italic: anObject [
	"Set the value of italic"

	italic := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> numFaces [
	"Answer the value of numFaces"

	^ numFaces
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> numFaces: anObject [
	"Set the value of numFaces"

	numFaces := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> postscriptName [
	"Answer the value of postscriptName"

	^ postscriptName
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> postscriptName: anObject [
	"Set the value of postscriptName"

	postscriptName := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> slant [
	"Answer the value of slant"

	^ slant
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> slantValue [
	^slantValue
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> stretch [
	"Answer the value of stretch"

	^ stretch
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> stretchValue [
	"Answer the value of stretchValue"

	^ stretchValue
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> stretchValue: anObject [
	"Set the value of stretchValue"

	stretchValue := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> style [
	"Answer the value of slant"

	^ slant
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> styleName [
	"Answer the value of styleName"

	^ styleName
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> styleName: anObject [
	"Set the value of styleName"

	styleName := anObject
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> styleNameExtracted [
	^styleNameExtracted
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> styleNameWithItalicForcedToBe: aString [
	| answer |
	answer := ''.
	stretch ifNotNil:[
		answer := answer ,stretch].
	(weight notNil "and:[weight asLowercase ~= 'medium']") 
		ifTrue:[
			answer := answer , ' ', weight].
	answer := answer , ' ', aString.
	answer := answer trimBoth.
	^answer
		 
	
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> styleNameWithWeightForcedToBe: aString [
	| answer |
	answer := ''.
	stretch ifNotNil:[
		answer := answer ,stretch].
	answer := answer , ' ', aString.
	slant ifNotNil:[
		answer := answer , ' ', slant].
	answer := answer trimBoth.
	^answer
		 
	
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> styleNameWithWeightForcedToBe: aString italicForcedToBe: aString2 [
	| answer |
	answer := ''.
	stretch ifNotNil:[
		answer := answer ,stretch].
	answer := answer , ' ', aString.
	answer := answer , ' ', aString2.
	answer := answer trimBoth.
	^answer
		 
	
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> validFamilyName [
	"answer the receiver's familyName, or an alternative 
	name to use if the familyName is invalid for some reason"
	
	(familyName copyWithout: $? ) 
		ifEmpty:[
			"workaround problem with FreeType 2.2.1 and MS Gothic, MS Mincho
			where familyName is not read correctly. This may be fixed in later versions
			of FreeType"
			self baseName asUppercase = 'MSGOTHIC'
				ifTrue:[
					index = 0 ifTrue:[^'MS Gothic'].
					index = 1 ifTrue:[^'MS PGothic'].
					index = 2 ifTrue:[^'MS UI Gothic']].
			self baseName asUppercase = 'MSMINCHO'
				ifTrue:[
					index = 0 ifTrue:[^'MS Mincho'].
					index = 1 ifTrue:[^'MS PMincho'].				
			^self baseName asUppercase, ' ', index asString]].
	^familyName
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> validStyleName [
	"answer the receiver's styleName, or an alternative 
	name to use if the styleName is invalid for some reason"
	
	
	(styleName copyWithout: $? ) 
		ifEmpty:[ | answer |
			"workaround problem with FreeType 2.2.1 and MS Gothic, MS Mincho
			where familyName is not read correctly. This may be fixed in later versions
			of FreeType"
			answer := ''.
			italic ifTrue:[answer := answer , 'Italic '].
			bold ifTrue:[answer := answer, 'Bold '].
			(italic or:[bold]) not ifTrue:[answer := answer, 'Regular '].
			^answer trimBoth].
	^styleName
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> weight [
	"Answer the value of weight"

	^ weight
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> weightValue [
	"Answer the value of weightValue"

	^ weightValue
]

{ #category : #accessing }
FreeTypeFileInfoAbstract >> weightValue: anObject [
	"Set the value of weightValue"

	weightValue := anObject
]
