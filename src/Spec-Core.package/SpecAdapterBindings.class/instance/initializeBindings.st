initialize
initializeBindings
	"This implementation is stupid, but it exposes all the container which need to be bound"
	
	^ bindings
		at: #ButtonAdapter				put: #ButtonAdapter;
		at: #CheckBoxAdapter			put: #CheckBoxAdapter;
		at: #ContainerAdapter			put: #ContainerAdapter;
		at: #DiffAdapter				put: #MorphicDiffAdapter;
		at: #ImageAdapter				put: #ImageAdapter;
		at: #LabelAdapter				put: #LabelAdapter;
		at: #ListAdapter				put: #ListAdapter;
		at: #IconListAdapter			put: #IconListAdapter;
		at: #FastTableAdapter			put: #FastTableAdapter;
		at: #DropListAdapter			put: #DropListAdapter;
		at: #MultiColumnListAdapter	put: #MultiColumnListAdapter;
		at: #MenuAdapter				put: #MenuAdapter;
		at: #MenuGroupAdapter		put: #MenuGroupAdapter;
		at: #MenuItemAdapter			put: #MenuItemAdapter;	
		at: #RadioButtonAdapter		put: #RadioButtonAdapter;
		at: #SliderAdapter				put: #SliderAdapter;
		at: #TabManagerAdapter		put: #TabManagerAdapter;
		at: #TabAdapter				put: #TabAdapter;
		at: #TextAdapter				put: #TextAdapter;
		at: #TextInputFieldAdapter		put: #TextInputFieldAdapter;
		at: #TransferAdapter			put: #TransferAdapter;
		at: #TreeAdapter				put: #TreeAdapter;
		at: #TreeColumnAdapter		put: #TreeColumnAdapter;
		at: #TreeNodeAdapter			put: #TreeNodeAdapter;		
		at: #WindowAdapter			put: #WindowAdapter;
		at: #TickingWindowAdapter	put: #TickingWindowAdapter;
		at: #DialogWindowAdapter		put: #DialogWindowAdapter;
		yourself