builder
buildSmartActionsShortcuts: aBuilder
	<keymap>

	(aBuilder shortcut: #smartSuggestions)
		category: #SmalltalkEditor
		default: $t meta
		do: [ :morph | morph smartSuggestions ]
		description: 'Browse suggestions based on selections'.

	(aBuilder shortcut: #smartSuggestions)
		category:  RubSmalltalkEditor name
		default: $t meta
		do: [ :morph | morph editor smartSuggestions ]
		description: 'Browse suggestions based on selections'.
