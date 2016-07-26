gt-inspector-extension
gtInspectorItems
	^ OrderedCollection new
			add: (#isRefinement -> self isRefinement);
			add: (#currentTextInfo -> self currentTextInfo);
			add: (#previousTextInfo -> self previousTextInfo);
			addAll: super gtInspectorItems;
			yourself