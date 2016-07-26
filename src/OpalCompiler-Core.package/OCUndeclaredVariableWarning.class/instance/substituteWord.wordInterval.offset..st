correcting
substituteWord: correctWord wordInterval: spot offset: o
	"Substitute the correctSelector into the (presuamed interactive) receiver."

	self requestor correctFrom: (spot first + o)
					to: (spot last + o)
					with: correctWord.

	^ o + correctWord size - spot size