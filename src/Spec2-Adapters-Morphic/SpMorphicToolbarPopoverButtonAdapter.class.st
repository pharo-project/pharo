"
I'm the adapter used to bridge a ToolBarPopoverButton and its morphic representation.
"
Class {
	#name : #SpMorphicToolbarPopoverButtonAdapter,
	#superclass : #SpMorphicToolbarButtonAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #execution }
SpMorphicToolbarPopoverButtonAdapter >> execute [

	self presenter newPopover
		addStyle: 'popoverButton';
		presenter: self presenter content value;
		popup
]
