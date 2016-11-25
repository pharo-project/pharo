access
methodRefList
	"Return a MethodReference for each message I can send."
	| list |
	list := OrderedCollection new.
	subscriptions do: [
		:subscriptionSet |
		
		subscriptionSet do: [ 	 :s | 
			s  ifNotNil: [list
						add: (RGMethodDefinition
								realClass: (s recipient class whichClassIncludesSelector: s selector)
								selector: s selector)]
							]
		].
	^ list