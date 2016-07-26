gt-inspector-extension
gtInspectorItems
	^ OrderedCollection new
			add: (#query -> self query);
			add: (#processors -> self processors);
			addAll: super gtInspectorItems;
			yourself