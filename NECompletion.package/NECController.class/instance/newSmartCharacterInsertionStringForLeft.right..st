keyboard
newSmartCharacterInsertionStringForLeft: left right: right
	((NECPreferences smartCharactersWithDoubleSpace includes: left) or: [
	(NECPreferences smartCharactersWithDoubleSpace includes: right)])
	ifTrue: [ 
		^ String with: left with: Character space with: Character space  with: right ].
	
	((NECPreferences smartCharactersWithSingleSpace includes: left) or: [
	(NECPreferences smartCharactersWithSingleSpace includes: right)])
	ifTrue: [ 
		^ String with: left with: Character space  with: right ].
	
	^ String with: left with: right