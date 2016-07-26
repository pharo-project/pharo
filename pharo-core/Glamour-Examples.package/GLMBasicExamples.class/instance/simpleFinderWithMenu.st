actions examples
simpleFinderWithMenu
	<glmBrowser: 'Browser menu' input: '$z'>
	"self new simpleFinderWithMenu openOn: $z"

	| finder |
	finder := GLMFinder new variableSizePanes.
	finder show: [:a | a list
			title: 'List';
			display: [:each | $a to: each];
			dynamicActionsOnSelection: [:list | self actionsFor: list ]].
	^ finder