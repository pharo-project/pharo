api
value: aNumber
	"<api: #integer min: #min max: #max getter:#value registration: #whenValueChanged:>"
	"Set the value in a range between min and max"
	
	value value == aNumber ifTrue: [ ^ self ].
	value value: aNumber.