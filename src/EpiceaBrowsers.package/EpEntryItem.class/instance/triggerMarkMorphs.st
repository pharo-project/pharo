private
triggerMarkMorphs

	^ Array streamContents: [:stream |
		self triggerItems do: [ :item |
			stream
				nextPut: (self morphFactory semiTransparent: item icon);
				nextPut: (self morphFactory secondaryStringFor: '»')
		] ]