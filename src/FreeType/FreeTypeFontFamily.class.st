"
FreeType font family
"
Class {
	#name : #FreeTypeFontFamily,
	#superclass : #FontFamilyAbstract,
	#category : 'FreeType-FontManager'
}

{ #category : #accessing }
FreeTypeFontFamily >> addMember: aFreeTypeFontFamilyMember [
	aFreeTypeFontFamilyMember family: self.
	members add: aFreeTypeFontFamilyMember
]

{ #category : #accessing }
FreeTypeFontFamily >> addMembersFromFileInfos: aCollectionOfFreeTypeFileInfo [
	
	
	aCollectionOfFreeTypeFileInfo do:[:aFileInfo | | member |
		member := FreeTypeFontFamilyMember fromFileInfo: aFileInfo.
		(self memberWithStyleName: member styleName)
			ifNil:[self addMember: member]].
	
]

{ #category : #'simulated members' }
FreeTypeFontFamily >> addSimulatedMembers [
	| membersBySlantAndStretch membersByWeightAndStretch |
	membersBySlantAndStretch := Dictionary new.
	members do:[:each| 
		(membersBySlantAndStretch 
			at: {each slantValue. each stretchValue} 
			ifAbsentPut:[OrderedCollection new]) 
				add: each].
	membersBySlantAndStretch keysAndValuesDo:[:key :col | | heaviest |
		heaviest := col ifNotEmpty:[col first].
		col do:[:each |
			heaviest weightValue < each weightValue
				ifTrue:[heaviest := each]].
		(heaviest weightValue between: (LogicalFont weightRegular - 50) and: (LogicalFont weightMedium + 50))
			ifTrue:[	members add: heaviest asSimulatedBold]].			
	membersByWeightAndStretch := Dictionary new.
	members do:[:each| | normalizedWeight |
		normalizedWeight := each weightValue.
		each weightValue = LogicalFont weightMedium ifTrue:[normalizedWeight := LogicalFont weightRegular].	
		"regular and medium weights are used interchangeably.
		For example, FreeSans has Regular-weightMedium(500), and Oblique-weightRegular(400).
		We don't want to simulate oblique-weightMedium(500) when a real 
		Oblique-weightMedium(500) exists, so we normalize any weightMedium(500)
		values to weightRegular(400) to prevent this happening" 	
		(membersByWeightAndStretch 
			at: {normalizedWeight. each stretchValue} 
			ifAbsentPut:[OrderedCollection new]) 
				add: each].	
	membersByWeightAndStretch keysAndValuesDo:[:key :col | | oblique regular |
		regular := col detect: [:each | each slantValue = 0] ifNone:[].
		oblique := col detect:[:each | each slantValue > 0] ifNone:[]. "oblique or italic"
		(oblique isNil and:[regular notNil]) 
			ifTrue:[
				regular simulated
					ifTrue:[members add: regular asSimulatedBoldOblique]
					ifFalse:[	members add: regular asSimulatedOblique]]]
]

{ #category : #initialization }
FreeTypeFontFamily >> initialize [
	super initialize.
	members := OrderedCollection new.
]

{ #category : #accessing }
FreeTypeFontFamily >> memberWithStyleName: aString [
	^members detect:[:each | each styleName = aString] ifNone:[]

]

{ #category : #'simulated members' }
FreeTypeFontFamily >> rebuildSimulatedMembers [
	"FOR TESTING ONLY"
	
	members := members reject:[:each| each simulated].
	self addSimulatedMembers.
]
