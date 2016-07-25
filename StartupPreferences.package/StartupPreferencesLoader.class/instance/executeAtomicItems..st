code execution
executeAtomicItems: aCollection
	"No sender but send from the generated script"

	'Load Startup Preferences'
		displayProgressFrom: 0 to: aCollection size
		during: [:bar | 
			aCollection doWithIndex: [:item :index | 
				bar current: index.
				(self add: item) execute ]]