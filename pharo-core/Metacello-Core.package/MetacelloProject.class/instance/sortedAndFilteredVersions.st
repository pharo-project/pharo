private
sortedAndFilteredVersions

		^(self map values asArray sort: [:a :b | a >= b ]) select: [:vrsn | (#(structural broken baseline) includes: vrsn blessing) not ].