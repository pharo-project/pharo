initialize
initializeBindings
	
	#(
		#ButtonAdapter				 #MorphicButtonAdapter
		#CheckBoxAdapter			 #MorphicCheckBoxAdapter
		#ContainerAdapter			 #MorphicContainerAdapter
		 #DiffAdapter				 #MorphicDiffAdapter
		 #DropListAdapter			 #MorphicDropListAdapter
		 #LabelAdapter				 #MorphicLabelAdapter
		 #ListAdapter				 #MorphicListAdapter
		 #IconListAdapter			 #MorphicIconListAdapter
		 #ImageAdapter			 #MorphicImageAdapter
		 #MultiColumnListAdapter	 #MorphicMultiColumnListAdapter
		 #MenuAdapter				 #MorphicMenuAdapter
		 #MenuGroupAdapter		 #MorphicMenuGroupAdapter
		 #MenuItemAdapter			 #MorphicMenuItemAdapter
		 #NewListAdapter			 #MorphicNewListAdapter
		 #RadioButtonAdapter		 #MorphicRadioButtonAdapter
		 #SliderAdapter				 #MorphicSliderAdapter
		 #TabManagerAdapter		 #MorphicTabManagerAdapter
		 #TabAdapter				 #MorphicTabAdapter
		 #TextAdapter				 #TxTextAdapter
		 #TextInputFieldAdapter		 #MorphicTextInputFieldAdapter
		 #TreeAdapter				 #MorphicTreeAdapter
		 #TreeColumnAdapter		 #MorphicTreeColumnAdapter
		 #TreeNodeAdapter			 #MorphicTreeNodeAdapter
		 #WindowAdapter			 #MorphicWindowAdapter
		 #TickingWindowAdapter	 #MorphicTickingWindowAdapter
		 #DialogWindowAdapter		 #MorphicDialogWindowAdapter
	
	) pairsDo: [ :key :val | bindings at: key put: val ].	
	
		