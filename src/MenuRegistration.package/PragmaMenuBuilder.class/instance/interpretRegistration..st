private
interpretRegistration: aRegistration 
	| root |
	root := MenuRegistration owner: self.
	aRegistration handOutItems do: [:item || node |
			node := item group
					ifNil: [ root item: item item ]
					ifNotNil: [:grp | root group: grp ].
			item with
				ifNil: [
					item action
						ifNil: [ 
							node 
								target: item target;
								arguments: item arguments;
								selector: item selector ]
						ifNotNil:[ 
							node 
								action: item action ]].
			node
				keyText: item keyText;
				help: item help;
				icon: item icon;
				order: item order;
				parent: item parent.
			item enabled
				ifNil: [ node enabledBlock: item enabledBlock ]
				ifNotNil: [:boolean | node enabled: boolean ].	
			item label
				ifNotNil: [ node label: item label ].
			item default ifNotNil: [ :d |
				node label: node label, ' (', d asString, ')'].
			item isWithSeparatorAfter
				ifTrue: [ node withSeparatorAfter ]]