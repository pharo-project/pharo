initialization
parseCompositionMappingFrom: stream
"
	self parseCompositionMapping
"
	| line fieldEnd point fieldStart compositions toNumber diacritical result |

	toNumber := [:quad | ('16r', quad) asNumber].

	Compositions := IdentityDictionary new: 2048.
	Decompositions := IdentityDictionary new: 2048.
	Diacriticals := IdentitySet new: 2048.

	[(line := stream nextLine) notNil] whileTrue: [
		fieldEnd := line indexOf: $; startingAt: 1.
		point := ('16r', (line copyFrom: 1 to: fieldEnd - 1)) asNumber.
		2 to: 6 do: [:i |
			fieldStart := fieldEnd + 1.
			fieldEnd := line indexOf: $; startingAt: fieldStart.
		].
		compositions := line copyFrom: fieldStart to: fieldEnd - 1.
		(compositions size > 0 and: [compositions first ~= $<]) ifTrue: [
			compositions := compositions substrings collect: toNumber.
			compositions size > 1 ifTrue: [
				diacritical := compositions first.
				Diacriticals add: diacritical.
				result := compositions second.
				(Decompositions includesKey: point) ifTrue: [
					self error: 'should not happen'.
				] ifFalse: [
					Decompositions at: point put: (Array with: diacritical with: result).
				].
				(Compositions includesKey: diacritical) ifTrue: [
					(Compositions at: diacritical) at: result put: point.
				] ifFalse: [
					Compositions at: diacritical
						put: (IdentityDictionary new at: result put: point; yourself).
				].
			].
		].
	].
