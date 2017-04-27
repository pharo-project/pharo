private
triggerMarkMorphs

	^ self triggerItems collect: [:item |
		self morphFactory semiTransparent: item icon ]