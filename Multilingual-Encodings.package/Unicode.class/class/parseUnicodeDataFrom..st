class initialization
parseUnicodeDataFrom: stream
	| line fieldEnd point fieldStart toNumber generalCategory decimalProperty loValue |

	toNumber := [:quad | ('16r', quad) asNumber].

	GeneralCategory := SparseLargeTable new: 16rE0080 chunkSize: 1024 arrayClass: Array base: 1 defaultValue:  (self classPool at: #Cn).
	DecimalProperty := SparseLargeTable new: 16rE0080 chunkSize: 32 arrayClass: Array base: 1 defaultValue: -1.

	loValue := (self classPool at: #Lo).
	16r3400 to: 16r4DB5 do: [:i | GeneralCategory at: i+1 put: loValue].
	16r4E00 to: 16r9FA5 do: [:i | GeneralCategory at: i+1 put: loValue].
	16rAC00 to: 16rD7FF do: [:i | GeneralCategory at: i+1 put: loValue].

	[(line := stream nextLine) notNil and: [line size > 0 ]] whileTrue: [
		fieldEnd := line indexOf: $; startingAt: 1.
		point := toNumber value: (line copyFrom: 1 to: fieldEnd - 1).
		point > 16rE007F ifTrue: [
			GeneralCategory zapDefaultOnlyEntries.
			DecimalProperty zapDefaultOnlyEntries.
			^ self].
		2 to: 3 do: [:i |
			fieldStart := fieldEnd + 1.
			fieldEnd := line indexOf: $; startingAt: fieldStart.
		].
		generalCategory := line copyFrom: fieldStart to: fieldEnd - 1.
		GeneralCategory at: point+1 put: (self classPool at: generalCategory asSymbol).
		generalCategory = 'Nd' ifTrue: [
			4 to: 7 do: [:i |
				fieldStart := fieldEnd + 1.
				fieldEnd := line indexOf: $; startingAt: fieldStart.
			].
			decimalProperty :=  line copyFrom: fieldStart to: fieldEnd - 1.
			DecimalProperty at: point+1 put: decimalProperty asNumber.
		].
	].
	GeneralCategory zapDefaultOnlyEntries.
	DecimalProperty zapDefaultOnlyEntries.
