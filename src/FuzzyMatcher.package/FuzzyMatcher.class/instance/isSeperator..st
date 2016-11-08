private
isSeperator: aCharacter
	"aCharacter isSeparator or: []"
	
	^  aCharacter == $_ or: [ aCharacter == $: ] 