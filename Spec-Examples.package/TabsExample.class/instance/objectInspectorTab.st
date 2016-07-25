private
objectInspectorTab
	|tab|
	tab := self newTab.
	tab
		label: 'Object inspector';
		icon: (tab iconNamed: #nautilusIcon);
		retrievingBlock: [ (EyeInspector new
				inspect: Object;
				buildWithSpec: #inspectorSpec) asWidget ].
			^tab