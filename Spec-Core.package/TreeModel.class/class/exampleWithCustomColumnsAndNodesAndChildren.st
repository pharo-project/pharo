examples
exampleWithCustomColumnsAndNodesAndChildren
	"self exampleWithCustomColumnsAndNodesAndChildren"

	| m col1 col2 |
	m := TreeModel new.
	m roots: #(#a #b #c #d).
	m
		rootNodeHolder: [ :item | 
			TreeNodeModel new
				content: item;
				children: [ (1 to: item asString first asciiValue) collect: [ :e | TreeNodeModel new content: e ] ];
				icon: (m iconNamed: #smallConfigurationIcon) ].
	m openWithSpec.
	col1 := TreeColumnModel new
		displayBlock: [ :node | node content asString ];
		headerAction: [ self halt ].
	col2 := TreeColumnModel new
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 1) asString ].
	m
		columns:
			{col1.
			col2};
		dragEnabled: true;
		dropEnabled: true;
		acceptDropBlock: [ :transfer :event :source :receiver | self halt ].
	col2
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 2) asString ]