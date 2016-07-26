error correction
substituteWord: correctWord wordInterval: spot offset: o
	"Substitute the correctSelector into the (presuamed interactive) receiver."

	requestor correctFrom: (spot first + o)
					to: (spot last + o)
					with: correctWord.

	requestorOffset := requestorOffset + correctWord size - spot size.
	^ o + correctWord size - spot size