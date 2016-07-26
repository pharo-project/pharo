others
watcher
	"self new watcher openOn: (Collection methods)"

	"You have to open the Watcher to see the effect."
	"GLMWatcherWindow uniqueInstance show."

	"<glmBrowser: 'Watcher' input: 'Collection methods'>"
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | a list title: 'Collection methods'; format: #selector].
	browser transmit
		from: #one;
		toWatcher;
		andShow: [ :a | a pharoMethod display: #sourceCode ].
	^ browser