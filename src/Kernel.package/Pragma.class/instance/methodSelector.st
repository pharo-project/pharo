accessing-method
methodSelector
	"API changed: see issue 18233. selector -> methodSelector & keyword -> selector"

	"Answer the selector of the method containing the pragma.
	 Do not confuse this with the selector of the pragma's message pattern."

	^ method selector