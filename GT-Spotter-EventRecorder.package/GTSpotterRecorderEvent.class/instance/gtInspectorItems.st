gt-inspector-extension
gtInspectorItems
	^ Array
		 with: (#eventTimestamp -> self timestamp) withAll: self associations