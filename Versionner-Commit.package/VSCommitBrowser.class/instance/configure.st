actions
configure
	| strategy options result |

	strategy := self unit commitStrategy copy. 
	options := strategy options.
	result := (TickDialogWindow 
		itemsList: options
		itemsHeaderName: '' 
		wrapBlockOrSelector: [ :item | item first ]
		title: 'Options') 
		selectItems: (options select: [ :each | strategy perform: each second ] );
		chooseFromOwner: window.
	result ifNil: [ ^ self ].
	(options difference: result) do: [ :each | strategy perform: each second asMutator with: false ]. 
	result do: [ :each | strategy perform: each second asMutator with: true ].
	
	self unit commitStrategy: strategy