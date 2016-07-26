private
displayBehavior: aClassName method: aSelector
	"^ (aClassName asString copy replaceAll: Character space  with: $/), '/methods #', aMethod selector"
	^ (self displayBehavior: aClassName), (self displayOperation: '»'), (self displayString: aSelector)
	"^ aClassName, ' >> #', aMethod selector"
	"^ aClassName, ' method: #', aMethod selector"
	"^ aClassName, ' method: (', (aMethod sourceCode printStringLimitedTo: 20), ')' "