private
displayBehavior: aClassName method: aSelector

	^ (self displayBehavior: aClassName), (self displayOperation: '»'), (self displayString: aSelector)