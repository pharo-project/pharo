I display a set of progress indicator in a list.
Special care is taken to update the view while the rest of the UI process is blocked.

Reset.
SystemProgressMorph reset.

Examples.
self show: 'Doing...' from: 500 to: 600 during: [ :bar |
	500 to: 600 do: [ :x | 
		bar current: x.
		(Delay forMilliseconds: 20) wait 
		"Just to slow it down so we can see what's going on" ] ].

self show: 'Doing...' from: 0 to: 50 during: [ :bar |
	0 to: 50 do: [ :x | 
		bar increment.
		bar label: x asString.
		(Delay forMilliseconds: 20) wait "Just to slow it down so we can see what's going on" ] ].