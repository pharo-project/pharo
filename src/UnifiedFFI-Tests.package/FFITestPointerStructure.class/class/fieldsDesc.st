field definition
fieldsDesc
" self rebuildFieldAccessors "
	^ #(
		byte one;
		long two;
		void *pointer;
		FFITestStructure *nestedPointer;
		)