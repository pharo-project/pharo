private
senders

	self selectedMethod 
		ifNotNil: [ :selector |.
			 	SystemNavigation new browseSendersOf: selector name: 'Senders of ', selector asString autoSelect: selector asString].