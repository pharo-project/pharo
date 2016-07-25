world menu
initializeDesktopCommandKeySelectors
	"Provide the starting settings for desktop command key selectors.  Answer the dictionary."

	"ActiveWorld initializeDesktopCommandKeySelectors"
	| dict |
	dict := IdentityDictionary new.
	self defaultDesktopCommandKeyTriplets do:
		[:trip | | messageSend |
			messageSend := MessageSend receiver: trip second selector: trip third.
			dict at: trip first put: messageSend].
	self setProperty: #commandKeySelectors toValue: dict.
	^ dict

