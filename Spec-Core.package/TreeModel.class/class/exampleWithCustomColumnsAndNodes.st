examples
exampleWithCustomColumnsAndNodes
	"self exampleWithCustomColumnsAndNodes"
	| m col1 col2 |

	m := TreeModel new.
	m roots: #(#a #b #c #d).
	m rootNodeHolder: [ :item | 
		TreeNodeModel new
			content: item;
			icon: (Smalltalk ui icons iconNamed: #smallConfigurationIcon)
	].
	
	m openWithSpec.
	
	col1 := TreeColumnModel new
		displayBlock: [ :node | node content asString ];
		headerLabel: 'Character'.
	col2 := TreeColumnModel new
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 1) asString ];
		headerLabel: 'Character +1';
		headerIcon: Smalltalk ui icons smallBackIcon.
	
	m
		columns: {col1. col2};
		dragEnabled: true;
		dropEnabled: true;
		acceptDropBlock: [ :transfer :event :source :receiver | self halt ].
		
	col2 
		headerLabel: 'Character +2';
		headerIcon: Smalltalk ui icons smallBackIcon;
		displayBlock: [ :node | (Character value: node content asString first asciiValue + 2) asString ].
	
	m rootNodeHolder: [ :item | 
		TreeNodeModel new
			content: (Character value: (item asString first asciiValue + 5)) asSymbol;
			icon: Smalltalk ui icons smallFullscreenIcon
	].

	col1 headerIcon: Smalltalk ui icons nautilusIcon.