protocol
state: aBoolean
	"Set if the radio is activated or not"

	<api: #boolean getter: #state>
	self canDeselectByClick
		ifTrue: [ stateHolder value: aBoolean ]
		ifFalse: [ self state
				ifFalse: [ stateHolder value: aBoolean ] ]