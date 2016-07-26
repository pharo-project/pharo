menu
loadMenuFor: mbInfo in: aMenu 
	"dynamic menu"
	aMenu target: self.
	((mbInfo commands 
		collect: [ :commandClass |commandClass target: mbInfo for: (versionList selectedItem)]
		thenSelect: [ :each | each canBeApplied ])
		asArray sort: [:a :b | a order <= b order ]) 
			do: [:cmd | 
				aMenu 
					add: cmd title 
					target: [	
						cmd executeWithDevWorkflow: project devWorkflow.
						self refreshConfiguration.
						self update ]
					selector:#value ].
	^ aMenu.