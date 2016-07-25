private
processorKeyBindingsDo: aBlock
	processorKeyBindings ifNotNil: [ 
		processorKeyBindings keys do: [ :each | 
			self kmDispatcher removeKeyCombination: each ] ].
	processorKeyBindings := Dictionary new.
	aBlock value