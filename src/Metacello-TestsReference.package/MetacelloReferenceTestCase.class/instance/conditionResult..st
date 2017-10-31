testing
conditionResult: resultString 
	"GemStone unconditionally prints Symbols with quotes, Squeak and Pharo do not"

	^((((((resultString 
		copyReplaceAll: '#preloadForAddOn' with: '#''preloadForAddOn''')
			copyReplaceAll: '#postloadForAddOn' with: '#''postloadForAddOn''')
				copyReplaceAll: '#baseline' with: '#''baseline''')
					copyReplaceAll: '#preloadForVersion' with: '#''preloadForVersion''')
						copyReplaceAll: '#postloadForVersion' with: '#''postloadForVersion''')
							copyReplaceAll: '#preloadForProject' with: '#''preloadForProject''')
								copyReplaceAll: '#postloadForProject' with: '#''postloadForProject'''