access
methodRefList
	"Return a MethodReference for each message I can send."
	| list |
	list := OrderedCollection new.
	subscriptions do: [ :s | 
		s recipient ifNotNil: [list
						add: (RGMethodDefinition
								realClass: (s recipient class whichClassIncludesSelector: s selector)
								selector: s selector)]
		].
	^ list