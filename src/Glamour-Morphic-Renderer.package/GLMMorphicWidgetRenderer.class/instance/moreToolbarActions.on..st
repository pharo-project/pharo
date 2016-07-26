private
moreToolbarActions: noIconActions on: aBrowser
	| menuMorph |
	menuMorph := MenuMorph new.
	noIconActions do: [ :each | 
		(menuMorph 
			add: each title
			target: each
			selector: #actOn:
			argument: aBrowser)
			enabled: (each isEnabledOn: aBrowser);
 			yourself].
	^ menuMorph