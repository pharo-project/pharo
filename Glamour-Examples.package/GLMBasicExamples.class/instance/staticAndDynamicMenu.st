actions examples
staticAndDynamicMenu
	<glmBrowser: 'Menus' input: '$z'>
	"self new staticAndDynamicMenu openOn: $z"

	| finder |
	finder := GLMFinder new.
	finder show: [ :a |
		a list
			title: 'List';
			display: [:each | $a to: each];
			dynamicActionsOnSelection: [:list | self actionsFor: list ];
			selectionAct: [self inform: 'static action'] entitled: 'Static action';
			selectionAct: [ self inform: 'Enabled for items < $d' ] 
				entitled: 'Enabled for items < $d' 
				enabledWhen: [ :presentation | presentation selection < $d ];
			selectionAct: [self inform: 'static sub action'] entitled: 'Static sub action' categorized: 'Sub menu'.
		a text
			title: 'Text';
			display: [:each | 'Character ', each asString];
			dynamicActionsOnSelection: [:text | OrderedCollection with: (GLMGenericAction new title: 'Print selection'; action: [text selectedText inspect ]; yourself) ];
			selectionAct: [self inform: 'Text static action'] entitled: 'Static action';
			selectionAct: [self inform: 'Text disabled action'] 
				entitled: 'Static disabled' 
				enabledWhen: [ false ]].
	^ finder