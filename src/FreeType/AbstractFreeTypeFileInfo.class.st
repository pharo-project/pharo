"
Abstract superclass for file infos for a FreeType font
"
Class {
	#name : 'AbstractFreeTypeFileInfo',
	#superclass : 'Object',
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
	#category : 'FreeType-FontManager',
	#package : 'FreeType',
	#tag : 'FontManager'
}

{ #category : 'testing' }
AbstractFreeTypeFileInfo class >> isAbstract [

	^ self == AbstractFreeTypeFileInfo
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> bold [
	"Answer the value of bold"

	^ bold
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> bold: anObject [
	"Set the value of bold"

	bold := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> extractAttributesFromNames [
	"derive values for the receiver's style(italic), weight, and stretch inst vars.
	Also set the familyGroupName and styleNameExtracted"

	| p |
	p := FreeTypeNameParser new
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
	stretch ifNotNil: [ styleNameExtracted := styleNameExtracted , stretch ].
	weight ifNotNil: [ styleNameExtracted := styleNameExtracted , ' ' , weight ]. "and:[weight asLowercase ~= 'medium']"
	slant ifNotNil: [ styleNameExtracted := styleNameExtracted , ' ' , slant ].
	styleNameExtracted := styleNameExtracted trimBoth.
	styleNameExtracted ifEmpty: [ styleNameExtracted := upright ifNil: [ 'Regular' ] ]
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> familyGroupName [
	"Answer the value of familyGroupName"

	^ familyGroupName
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> familyName [
	"Answer the value of familyName"

	^ familyName
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> familyName: anObject [
	"Set the value of familyName"

	familyName := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> fixedWidth [
	"Answer the value of fixedWidth"

	^ fixedWidth
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> fixedWidth: anObject [
	"Set the value of fixedWidth"

	fixedWidth := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> index [
	"Answer the value of index"

	^ index
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> index: anObject [
	"Set the value of index"

	index := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> isBolderThan: val [
	^self weightValue >= val
]

{ #category : 'testing' }
AbstractFreeTypeFileInfo >> isEmbedded [
	^false
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> isItalicOrOblique [
	^self slantValue > 0
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> italic [
	"Answer the value of italic"

	^ italic
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> italic: anObject [
	"Set the value of italic"

	italic := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> numFaces [
	"Answer the value of numFaces"

	^ numFaces
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> numFaces: anObject [
	"Set the value of numFaces"

	numFaces := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> postscriptName [
	"Answer the value of postscriptName"

	^ postscriptName
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> postscriptName: anObject [
	"Set the value of postscriptName"

	postscriptName := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> slant [
	"Answer the value of slant"

	^ slant
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> slantValue [
	^slantValue
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> stretch [
	"Answer the value of stretch"

	^ stretch
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> stretchValue [
	"Answer the value of stretchValue"

	^ stretchValue
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> stretchValue: anObject [
	"Set the value of stretchValue"

	stretchValue := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> style [
	"Answer the value of slant"

	^ slant
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> styleName [
	"Answer the value of styleName"

	^ styleName
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> styleName: anObject [
	"Set the value of styleName"

	styleName := anObject
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> styleNameExtracted [
	^styleNameExtracted
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> styleNameWithItalicForcedToBe: aString [

	| answer |
	answer := ''.
	stretch ifNotNil: [ answer := answer , stretch ].
	weight ifNotNil: [ answer := answer , ' ' , weight ]. "and:[weight asLowercase ~= 'medium']"
	answer := answer , ' ' , aString.
	answer := answer trimBoth.
	^ answer
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> styleNameWithWeightForcedToBe: aString [
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

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> styleNameWithWeightForcedToBe: aString italicForcedToBe: aString2 [
	| answer |
	answer := ''.
	stretch ifNotNil:[
		answer := answer ,stretch].
	answer := answer , ' ', aString.
	answer := answer , ' ', aString2.
	answer := answer trimBoth.
	^answer
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> validFamilyName [
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

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> validStyleName [
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

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> weight [
	"Answer the value of weight"

	^ weight
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> weightValue [
	"Answer the value of weightValue"

	^ weightValue
]

{ #category : 'accessing' }
AbstractFreeTypeFileInfo >> weightValue: anObject [
	"Set the value of weightValue"

	weightValue := anObject
]
