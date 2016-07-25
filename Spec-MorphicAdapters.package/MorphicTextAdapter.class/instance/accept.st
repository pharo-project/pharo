spec protocol
accept
	"We use RubScrolledTextMorph>>#acceptContents but we keep accept as method name for compatibility"
	self widgetDo: [ :w | w acceptContents ]