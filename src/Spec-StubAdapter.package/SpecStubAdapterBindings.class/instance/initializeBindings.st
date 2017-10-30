initialize
initializeBindings
	
	^ bindings
		at: #ButtonAdapter				put: #SpecStubButtonAdapter;
		at: #CheckBoxAdapter			put: #SpecStubCheckBoxAdapter;
		at: #ContainerAdapter			put: #SpecStubContainerAdapter;
		at: #DiffAdapter				put: #SpecStubDiffAdapter;
		at: #DropListAdapter			put: #SpecStubDropListAdapter;
		at: #LabelAdapter				put: #SpecStubLabelAdapter;
		at: #ListAdapter				put: #SpecStubListAdapter;
		at: #IconListAdapter			put: #SpecStubIconListAdapter;
		at: #FastTableAdapter			put: #SpecStubFastTableAdapter;
		at: #ImageAdapter				put: #SpecStubImageAdapter;
		at: #MultiColumnListAdapter	put: #SpecStubMultiColumnListAdapter;
		at: #MenuAdapter				put: #SpecStubMenuAdapter;
		at: #MenuGroupAdapter			put: #SpecStubMenuGroupAdapter;
		at: #MenuItemAdapter			put: #SpecStubMenuItemAdapter;
		at: #RadioButtonAdapter		put: #SpecStubRadioButtonAdapter;
		at: #SliderAdapter				put: #SpecStubSliderAdapter;
		at: #TabManagerAdapter		put: #SpecStubTabManagerAdapter;
		at: #TabAdapter					put: #SpecStubTabAdapter;
		at: #TextAdapter				put: #SpecStubTextAdapter;
		at: #TextInputFieldAdapter	put: #SpecStubTextInputFieldAdapter;
		at: #TransferAdapter			put: #SpecStubTransferAdapter;
		at: #TreeAdapter				put: #SpecStubTreeAdapter;
		at: #TreeColumnAdapter		put: #SpecStubTreeColumnAdapter;
		at: #TreeNodeAdapter			put: #SpecStubTreeNodeAdapter;
		at: #WindowAdapter				put: #SpecStubWindowAdapter;
		at: #TickingWindowAdapter	put: #SpecStubTickingWindowAdapter;
		at: #DialogWindowAdapter		put: #SpecStubDialogWindowAdapter;
		yourself