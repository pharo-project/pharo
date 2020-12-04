Class {
	#name : #ScrollbarTest,
	#superclass : #SliderTest,
	#category : #'Morphic-Tests-Widgets'
}

{ #category : #helpers }
ScrollbarTest >> createSlider [

	^ ScrollBar new
		extent: 240 @ 8;
		yourself
]
