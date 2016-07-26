*GT-InspectorExtensions-Core
gtInspectorCommentIn: composite
	"This provides an editable comment of the current class"
	<gtInspectorPresentationOrder: 20>
	composite text 
		title: 'Comment';
		display: [ self comment ];
		act: [ :text | 
			self comment: text text stamp: Author changeStamp]
			icon: GLMUIThemeExtraIcons glamorousAccept 
			on: $s
			entitled: 'Accept'