settings
uiFetchPacks
	| choosenPacks remotePacks |

	UIManager default 
		informUser: 'Retrieving available packs from remote repository' 
		during: [ remotePacks := self fetchPacks ].
		
	choosenPacks := (TickDialogWindow 
		itemsList: remotePacks
		itemsHeaderName: nil 
		wrapBlockOrSelector: #name 
		title: 'Remote available icon packs')
		chooseFromOwner: World.
	choosenPacks ifNil: [ ^ self ].
	UIManager default informUserDuring: [ :bar | 
		choosenPacks do: [ :each |
			bar label: 'Retrieving "', each name, '" icon set from remote repository'.
			each downloadFromUrl ]] 	