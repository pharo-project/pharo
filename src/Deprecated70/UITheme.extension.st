Extension { #name : #UITheme }

{ #category : #'*Deprecated70' }
UITheme >> taskbarButtonLabelColorForCollapsed: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is collapsed."

	self
		deprecated: 'Please use #taskbarItemLabelColorForCollapsed: instead'
		transformWith: '`@receiver taskbarButtonLabelColorForCollapsed: `@statements' -> '`@receiver taskbarItemLabelColorForCollapsed: `@statements'.
	^ self taskbarItemLabelColorForCollapsed: aButton
]

{ #category : #'*Deprecated70' }
UITheme >> taskbarButtonLabelColorForExpanded: aButton [
	"Answer the colour for the label of the given taskbar button, given that the corresponding window is expanded."

	self
		deprecated: 'Please use #taskbarItemLabelColorForExpanded: instead'
		transformWith: '`@receiver taskbarButtonLabelColorForExpanded: `@statements' -> '`@receiver taskbarItemLabelColorForExpanded: `@statements'.
	^ self taskbarItemLabelColorForExpanded: aButton
]
