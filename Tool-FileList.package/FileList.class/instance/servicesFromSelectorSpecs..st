own services
servicesFromSelectorSpecs: symbolArray
	"Answer an array of services represented by the incoming symbols, eliminating any that do not have a currently-registered service.  Pass the symbol #- along unchanged to serve as a separator between services"

	"FileList new servicesFromSelectorSpecs: #(fileIn: fileIntoNewChangeSet: browseChangesFile:)"

	| registeredServices services |
	services := OrderedCollection new.
	registeredServices := self class allRegisteredServices.
	symbolArray
		do: [ :sel | 
			sel == #-
				ifTrue: [ services add: sel ]
				ifFalse: [ registeredServices detect: [ :each | each selector = sel ] ifFound: [ :service | services add: service ] ] ].
	^ services