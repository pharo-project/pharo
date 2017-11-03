"
FreeType FontFamily member  
"
Class {
	#name : #FreeTypeFontFamilyMember,
	#superclass : #FontFamilyMemberAbstract,
	#instVars : [
		'fileInfo',
		'stretchName',
		'stretchValue',
		'weightName',
		'weightValue',
		'slantName',
		'slantValue',
		'simulated'
	],
	#category : #FreeType-FontManager
}

{ #category : #'instance creation' }
FreeTypeFontFamilyMember class >> fromFileInfo: aFreeTypeFileInfo [
	
	^self new
		fileInfo: aFreeTypeFileInfo;
		simulated: false;
		styleName: aFreeTypeFileInfo styleNameExtracted;
		stretchName: aFreeTypeFileInfo stretch;
		stretchValue: aFreeTypeFileInfo stretchValue;
		weightName: aFreeTypeFileInfo weight;
		weightValue: aFreeTypeFileInfo weightValue;
		slantName: aFreeTypeFileInfo slant;
		slantValue: aFreeTypeFileInfo slantValue;
		yourself
		
]

{ #category : #comparing }
FreeTypeFontFamilyMember >> <= aFreeTypeFontFamilyMember [
	^self sortValue <= aFreeTypeFontFamilyMember sortValue
]

{ #category : #copying }
FreeTypeFontFamilyMember >> asSimulatedBold [
	^self copy 
		weightValue: LogicalFont weightBold;
		styleName: (fileInfo styleNameWithWeightForcedToBe: 'Bold');
		simulated: true;
		yourself
]

{ #category : #copying }
FreeTypeFontFamilyMember >> asSimulatedBoldOblique [
	^self copy 
		slantValue: LogicalFont slantItalic; "treat italic and oblique the same"
		weightValue:LogicalFont weightBold;
		styleName: (fileInfo styleNameWithWeightForcedToBe: 'Bold' italicForcedToBe: 'Oblique');
		simulated: true;
		yourself
]

{ #category : #copying }
FreeTypeFontFamilyMember >> asSimulatedOblique [
	^self copy 
		slantValue: LogicalFont slantItalic;  "treat italic and oblique the same"
		styleName: (fileInfo styleNameWithItalicForcedToBe: 'Oblique');
		simulated: true;
		yourself
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> fileInfo [
	"Answer the value of fileInfo"

	^ fileInfo
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> fileInfo: anObject [
	"Set the value of fileInfo"

	fileInfo := anObject
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> simulated [
	"Answer the value of simulated"

	^ simulated
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> simulated: anObject [
	"Set the value of simulated"

	simulated := anObject
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> slantName [
	"Answer the value of slantName"

	^ slantName
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> slantName: anObject [
	"Set the value of slantName"

	slantName := anObject
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> slantValue [
	"Answer the value of slantValue"

	^ slantValue
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> slantValue: anObject [
	"Set the value of slantValue"

	slantValue := anObject
]

{ #category : #comparing }
FreeTypeFontFamilyMember >> sortValue [
	| v normalizedWeight | 
	normalizedWeight := weightValue.
	normalizedWeight = LogicalFont weightMedium 
		ifTrue:["sort medium and regular weights as though they were the same"
			normalizedWeight := LogicalFont weightRegular]. 
	v :=self simulated ifTrue:[10000] ifFalse:[0].
	v := v + (stretchValue * 1000).
	v := v + (normalizedWeight).
	v := v + (slantValue).
	^v

]

{ #category : #accessing }
FreeTypeFontFamilyMember >> stretchName [
	"Answer the value of stretchName"

	^ stretchName
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> stretchName: anObject [
	"Set the value of stretchName"

	stretchName := anObject
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> stretchValue [
	"Answer the value of stretchValue"

	^ stretchValue
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> stretchValue: anObject [
	"Set the value of stretchValue"

	stretchValue := anObject
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> weightName [
	"Answer the value of weightName"

	^ weightName
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> weightName: anObject [
	"Set the value of weightName"

	weightName := anObject
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> weightValue [
	"Answer the value of weightValue"

	^ weightValue
]

{ #category : #accessing }
FreeTypeFontFamilyMember >> weightValue: anObject [
	"Set the value of weightValue"

	weightValue := anObject
]
