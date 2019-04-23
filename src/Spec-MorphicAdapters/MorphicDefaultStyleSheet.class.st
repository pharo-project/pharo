Class {
	#name : #MorphicDefaultStyleSheet,
	#superclass : #MorphicStyleSheet,
	#category : #'Spec-MorphicAdapters-StyleSheet'
}

{ #category : #defaults }
MorphicDefaultStyleSheet >> defaultButtonFontFamily [

	^ Smalltalk ui theme buttonFont familyName
]

{ #category : #defaults }
MorphicDefaultStyleSheet >> defaultButtonFontSize [

	^ Smalltalk ui theme buttonFont pointSize
]

{ #category : #initialization }
MorphicDefaultStyleSheet >> initialize [ 

	super initialize.
	self  
		addClass: #Button with: { MorphicStyleSheetExtent newExtent: 80@25 };
		addClass: #CheckBox with: { MorphicStyleSheetExtent newExtent: 15@25 hResizing: true };
		addClass: #RadioButton with: { MorphicStyleSheetExtent newExtent: 15@25 };
		addClass: #DropList with: { MorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #List with: { MorphicStyleSheetExtent newExtent: 150@100 hResizing: true };
		addClass: #Label with: { MorphicStyleSheetExtent newExtent: 50@25 hResizing: true };
		addClass: #Link with: { MorphicStyleSheetExtent newExtent: 50@25 hResizing: true };
		addClass: #Text with: { MorphicStyleSheetExtent newExtent: 150@100 hResizing: true vResizing: true};
		addClass: #TextInputField with: { MorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #NumberInputField with: { MorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		"Bars"
		addClass: #ActionBar with: { MorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #ActionButton with: { MorphicStyleSheetExtent newExtent: 60@25 }		
]
