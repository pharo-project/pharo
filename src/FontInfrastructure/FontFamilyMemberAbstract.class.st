"
Abstract member for a font family
"
Class {
	#name : #FontFamilyMemberAbstract,
	#superclass : #Object,
	#instVars : [
		'family',
		'styleName'
	],
	#category : #FontInfrastructure
}

{ #category : #converting }
FontFamilyMemberAbstract >> asLogicalFontOfPointSize: pointSize [
	^LogicalFont
		familyName: self family familyName 
		pointSize: pointSize 
		stretchValue: self stretchValue
		weightValue: self weightValue
		slantValue: self slantValue
]

{ #category : #comparing }
FontFamilyMemberAbstract >> closenessVector [
	^self closenessVectorForStretch: self stretchValue slant: self slantValue weight: self weightValue
]

{ #category : #comparing }
FontFamilyMemberAbstract >> closenessVectorForStretch: stretch slant: slant weight: weight [
	| normalizedSlant |
	normalizedSlant := slant.
	normalizedSlant ~= 0 ifTrue:[
		 "treat italic and oblique as though they were they same"
		normalizedSlant := LogicalFont slantItalic].
	^{(stretch - LogicalFont stretchRegular) * 11. slant * 7. ((weight - LogicalFont weightRegular) / 100) * 5}
]

{ #category : #accessing }
FontFamilyMemberAbstract >> family [
	^family
]

{ #category : #accessing }
FontFamilyMemberAbstract >> family: aFontFamily [
	family := aFontFamily
]

{ #category : #comparing }
FontFamilyMemberAbstract >> isCloserMatchThan: otherMember toStretch: inputStretch weight: inputWeight slant: inputSlant [ 
	| inputVector vector otherVector distance otherDistance dotProduct otherDotProduct |
	
	inputVector := self closenessVectorForStretch: inputStretch slant: inputSlant weight: inputWeight.
	vector := self closenessVector.
	otherVector := otherMember closenessVector.
	distance := (((inputVector first - vector first) raisedTo: 2) +
			((inputVector second - vector second) raisedTo: 2) +
			((inputVector third - vector third) raisedTo: 2)) sqrt.
	otherDistance := (((inputVector first - otherVector first) raisedTo: 2) +
			((inputVector second - otherVector second) raisedTo: 2) +
			((inputVector third - otherVector third) raisedTo: 2)) sqrt.
	distance < otherDistance ifTrue:[^true].
	distance > otherDistance ifTrue:[^false].	
	dotProduct := (inputVector first * vector first) +
				(inputVector second * vector second) +
				(inputVector third * vector third).
	otherDotProduct := (inputVector first * otherVector first) +
				(inputVector second * otherVector second) +
				(inputVector third * otherVector third).
	dotProduct > otherDotProduct ifTrue:[^true].
	dotProduct < otherDotProduct ifTrue:[^false].	
	vector first > otherVector first ifTrue:[^true].
	vector first < otherVector first ifTrue:[^false].
	vector second > otherVector second ifTrue:[^true].
	vector second < otherVector second ifTrue:[^false].
	vector third > otherVector third ifTrue:[^true].
	vector third < otherVector third ifTrue:[^false].
	^false 
]

{ #category : #accessing }
FontFamilyMemberAbstract >> slantValue [
	self subclassResponsibility
]

{ #category : #accessing }
FontFamilyMemberAbstract >> stretchValue [
	self subclassResponsibility
]

{ #category : #accessing }
FontFamilyMemberAbstract >> styleName [
	"Answer the value of styleName"

	^ styleName
]

{ #category : #accessing }
FontFamilyMemberAbstract >> styleName: anObject [
	"Set the value of styleName"

	styleName := anObject
]

{ #category : #accessing }
FontFamilyMemberAbstract >> weightValue [
	self subclassResponsibility
]
