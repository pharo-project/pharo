gt-inspector-extension
gtInspectorItems
	^ OrderedCollection new
			add: (#categoryLabel -> self categoryLabel);
			add: (#elementCount -> self elementCount);
			addAll: super gtInspectorItems;
			yourself