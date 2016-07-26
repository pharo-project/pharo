factory
buildWidget
	^ SpecTreeNodeModel new
		setItemFromBlock: self itemBlock;
		icon: self icon;
		setChildren: self childrenBlock;
		hasContents: self hasChildrenBlock;
		model: self container;
		nodeModel: self model