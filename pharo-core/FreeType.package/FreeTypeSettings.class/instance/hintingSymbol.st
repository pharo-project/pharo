accessing
hintingSymbol
	"#Full, #Light, #Normal or #None"
	^ self monoHinting 
		ifTrue: [#Full]
		ifFalse: [
			self lightHinting 
				ifTrue: [#Light]
				ifFalse: [
					self hinting 
						ifTrue: [#Normal]
						ifFalse: [#None]]]
