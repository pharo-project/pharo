"
I am used to link the spec-adapter names to the morphic adapters
"
Class {
	#name : #MorphicAdapterBindings,
	#superclass : #SpecAdapterBindings,
	#category : #'Spec-MorphicAdapters-Support'
}

{ #category : #fonts }
MorphicAdapterBindings >> defaultFont [

	^ StandardFonts defaultFont
]

{ #category : #initialize }
MorphicAdapterBindings >> initializeBindings [
	
	^ bindings
		at: #ButtonAdapter				put: #MorphicButtonAdapter;
		at: #CheckBoxAdapter			put: #MorphicCheckBoxAdapter;
		at: #ContainerAdapter			put: #MorphicContainerAdapter;
		at: #DiffAdapter				put: #MorphicDiffAdapter;
		at: #DropListAdapter			put: #MorphicDropListAdapter;
		at: #LabelAdapter				put: #MorphicLabelAdapter;
		at: #ListAdapter				put: #MorphicListAdapter;
		at: #IconListAdapter			put: #MorphicIconListAdapter;
		at: #FastTableAdapter			put: #MorphicFastTableAdapter;
		at: #ImageAdapter			put: #MorphicImageAdapter;
		at: #MultiColumnListAdapter	put: #MorphicMultiColumnListAdapter;
		at: #MenuAdapter				put: #MorphicMenuAdapter;
		at: #MenuGroupAdapter		put: #MorphicMenuGroupAdapter;
		at: #MenuItemAdapter			put: #MorphicMenuItemAdapter;
		at: #RadioButtonAdapter		put: #MorphicRadioButtonAdapter;
		at: #SliderAdapter				put: #MorphicSliderAdapter;
		at: #TabManagerAdapter		put: #MorphicTabManagerAdapter;
		at: #TabAdapter				put: #MorphicTabAdapter;
		at: #TextAdapter				put: #MorphicTextAdapter;
		at: #TextInputFieldAdapter		put: #MorphicTextInputFieldAdapter;
		at: #TransferAdapter			put: #MorphicTransferAdapter;
		at: #TreeAdapter				put: #MorphicTreeAdapter;
		at: #TreeColumnAdapter		put: #MorphicTreeColumnAdapter;
		at: #TreeNodeAdapter			put: #MorphicTreeNodeAdapter;
		at: #WindowAdapter			put: #MorphicWindowAdapter;
		at: #TickingWindowAdapter	put: #MorphicTickingWindowAdapter;
		at: #DialogWindowAdapter		put: #MorphicDialogWindowAdapter;
		yourself
]
