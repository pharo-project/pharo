populating browser
triggerMarkWidgets

	^ self triggerItems flatCollect: [ :item |
		"self tagWidgetFor2: '»' color: Color yellow muchLighter muchLighter"
		{ AlphaImageMorph new image: item icon; alpha: 0.5; yourself.
		"item itemIcon asMorph. "
		(self smallStringWidgetFor: '»' color: Color yellow muchLighter muchLighter)
			layoutInset: 0@0;
			yourself } ]