drawing
newContentMorph
	|pluggableTextMorph|
	pluggableTextMorph := super newContentMorph.

	pluggableTextMorph 
		autoAccept: false;
		askBeforeDiscardingEdits: false;
		setProperty: #wantsFrameAdornments toValue: false;
		changedAction: [:aString| self contentChanged: aString asString];
		eventHandler: MorphicEventHandler new.
		
	pluggableTextMorph on: #keyStroke send: #keyStroke:fromMorph: to: self.
	^ pluggableTextMorph