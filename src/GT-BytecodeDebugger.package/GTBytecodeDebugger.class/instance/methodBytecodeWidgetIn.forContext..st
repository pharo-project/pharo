building presentations
methodBytecodeWidgetIn: aComposite forContext: aContext

	^ aComposite tree
		title: 'Bytecode';
		rootsExpanded;
		display: [ aContext method symbolicBytecodes ];
		children: [ :each | each isArray ifTrue: [each] ifFalse: #() ];
		format: [ :each | self formatBytecode: each ];
		onChangeOfPort: #selection act: [ :bytecodes | 
			self codePresentation 
				selectionInterval: (bytecodes selection
					ifNil: [ 1 to: 0 ] 
					ifNotNil: [:selectedBytecode| selectedBytecode sourceInterval]) ] 