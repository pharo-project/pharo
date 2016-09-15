private
displayClass: aClassName protocol: aProtocolName

	^ (self displayBehavior: aClassName), (self displayOperation: 'protocol'), (self displayString: aProtocolName)
