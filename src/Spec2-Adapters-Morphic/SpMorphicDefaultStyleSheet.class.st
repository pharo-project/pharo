Class {
	#name : #SpMorphicDefaultStyleSheet,
	#superclass : #SpMorphicStyleSheet,
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #defaults }
SpMorphicDefaultStyleSheet >> defaultButtonFontFamily [
	^ self theme buttonFont familyName
]

{ #category : #defaults }
SpMorphicDefaultStyleSheet >> defaultButtonFontSize [
	^ self theme buttonFont pointSize
]

{ #category : #initialization }
SpMorphicDefaultStyleSheet >> initialize [ 

	super initialize.
	self  
		addClass: #Button with: { SpMorphicStyleSheetExtent newExtent: 80@25 };
		addClass: #CheckBox with: { SpMorphicStyleSheetExtent newExtent: 15@25 hResizing: true };
		addClass: #RadioButton with: { SpMorphicStyleSheetExtent newExtent: 15@25 };
		addClass: #DropList with: { SpMorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #List with: { SpMorphicStyleSheetExtent newExtent: 150@100 hResizing: true vResizing: true};
		addClass: #Label with: { SpMorphicStyleSheetExtent newExtent: 50@25 hResizing: true };
		addClass: #Link with: { SpMorphicStyleSheetExtent newExtent: 50@25 hResizing: true };
		addClass: #Text with: { SpMorphicStyleSheetExtent newExtent: 150@100 hResizing: true vResizing: true};
		addClass: #TextInputField with: { SpMorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #NumberInputField with: { SpMorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #Slider with: { SpMorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		"Bars"
		addClass: #ActionBar with: { SpMorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #MenuBar with: { SpMorphicStyleSheetExtent newExtent: 150@25 hResizing: true };
		addClass: #ActionButton with: { SpMorphicStyleSheetExtent newExtent: 60@25 }		
]
