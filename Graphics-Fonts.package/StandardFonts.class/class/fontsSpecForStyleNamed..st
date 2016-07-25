styles
fontsSpecForStyleNamed: aSymbol
	| k small regular big |
	k := StrikeFont defaultFontKey.
	small := self smallPointSizeForStyleNamed: aSymbol.
	regular := self pointSizeForStyleNamed: aSymbol.
	big := self bigPointSizeForStyleNamed: aSymbol.
	^ {
			{#defaultFont:. k. regular }. 
			{#codeFont:. k. regular }.
			{#listFont:. k. regular }. 
			{#menuFont:. k. regular }. 
			{#windowTitleFont:. k. big }. 
			{#balloonFont:. k. small }. 
			{#haloFont:. k. small }.
			{#buttonFont:. k. regular } }
