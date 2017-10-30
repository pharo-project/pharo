tests
testAddingToCharacterSet

	| cs wcs wc |
	cs := CharacterSet newFrom: 'aeiouy'.
	wcs := cs copy.
	wc := 4452 asCharacter.
	
	"adding a WideCharacter to an ordinary CharacterSet should turn it into a WideCharacterSet"
	wcs add: wc.

	self should: [wcs size = (cs size + 1)] description: 'We just added a Character, size should be increased by one'.
	self shouldnt: [wcs = cs] description: 'We just added a Character, sets should not be equal'.
	self shouldnt: [cs = wcs] description: 'We just added a Character, sets should not be equal'.
	self should: [cs allSatisfy: [:char | wcs includes: char]] description: 'Each character of the original CharacterSet should be included in the WideCharacterSet'.
	self should: [wcs hasWideCharacters] description: 'We just added a WideCharacter, so this WideCharacterSet definitely has one'.
	self should: [wcs includes: wc] description: 'We just added this WideCharacter, so this WideCharacterSet should include it'.
	
	wcs add: wc.
	self should: [wcs size = (cs size + 1)] description: 'We just added a Character already included in the set, size should be unchanged'.
	
	wcs remove: wc.
	self should: [wcs size = cs size] description: 'We added then removed a Character, now size should be equal to original'.
	self shouldnt: [wcs hasWideCharacters] description: 'We just removed the only WideCharacter, so this WideCharacterSet definitely has no WideCharacter'.
	
	self should: [wcs = cs] description: 'A WideCharacterSet can be equal to an Ordinary CharacterSet'.
	self should: [cs = wcs] description: 'An ordinary CharacterSet can be equal to a WideCharacterSet'.
	self should: [cs hash = wcs hash] description: 'If some objects are equal, then they should have same hash code'.
	
	