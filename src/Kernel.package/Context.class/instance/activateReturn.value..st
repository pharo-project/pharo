private
activateReturn: aContext value: value
	"Activate 'aContext return: value' in place of self, so execution will return to aContext's sender"

	^ self
		activateMethod: Context theReturnMethod
		withArgs: {value}
		receiver: aContext
		class: aContext class