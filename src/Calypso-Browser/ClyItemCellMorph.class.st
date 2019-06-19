"
I represent simple label based cell in QueryView table.
You can decorate this lable using special color or font styles: 
	cellMorph label emphasis: TextEmphasis italic emphasisCode.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	label:		<Morph>
"
Class {
	#name : #ClyItemCellMorph,
	#superclass : #FTCellMorph,
	#instVars : [
		'label'
	],
	#category : #'Calypso-Browser-Table'
}

{ #category : #building }
ClyItemCellMorph >> build [
	self addMorphBack: label
]

{ #category : #initialization }
ClyItemCellMorph >> initialize [
	super initialize.
	label := StringMorph new.
]

{ #category : #accessing }
ClyItemCellMorph >> label [
	^ label
]
