*Morphic-Base
menuFrom: specBlock

	^ [ specBlock value asMenuMorph ]
			on: Error
			do: [
				self reset.
				self fallbackMenu ].