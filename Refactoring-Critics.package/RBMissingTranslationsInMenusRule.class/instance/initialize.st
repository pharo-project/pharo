initialization
initialize
	super initialize.
	self matcher
		matchesAnyOf: #(
			'`@menu add: `#label action: `#sym'
			'`@menu add: `#label selector: `#sym arguments: `@stuff'
			'`@menu add: `#label subMenu: `@stuff target: `@targ selector: `#sel argumentList: `@args'
			'`@menu add: `#label subMenu: `@stuff'
			'`@menu add: `#label target: `@targ action: `#sel'
			'`@menu add: `#label target: `@targ selector `#sel argument: `@arg'
			'`@menu add: `#label target: `@targ selector `#sel arguments: `@arg'
			'`@menu add: `#label target: `@targ selector `#sel'
			'`@menu addList: `{ :n | n isLiteral and: [ n value isArray and: [ n value anySatisfy: [ :row | (row isKindOf: Array) and: [ row first isLiteral ] ] ] ] }'
			'`@menu addTitle: `#label updatingSelector: `#sel updateTarget: `@targ'
			'`@menu addTitle: `#label'
			'`@menu addWithLabel: `#label enablement: `#esel action: `#sel'
			'`@menu addWithLabel: `#label enablementSelector: `#esel target: `@targ selector: `#sel argumentList: `@args'
			'`@menu balloonTextForLastItem: `#label'
			'`@menu labels: `#lit lines: `@lines selections: `@sels'
			'`@menu title: `#title' )
		do: [ :node :answer | node ]