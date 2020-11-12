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
