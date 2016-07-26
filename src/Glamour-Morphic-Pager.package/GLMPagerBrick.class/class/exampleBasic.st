examples
exampleBasic
	|pager adapter|
	adapter := GLMPagerModel new.
	pager :=	GLMPagerBrick new.
	pager pagerModel: adapter.
	pager pushPane: (GLMLabelBrick new color: Color lightRed; text: 'Hello'; textPosition: #center).
	"pager pushPane: (GLMBrick new color: Color green).
	pager pushPane: (GLMBrick new color: Color yellow).
	pager pushPane: (GLMBrick new color: Color blue).
	pager pushPane: (GLMBrick new color: Color pink)."
	
	pager height: 600.
	pager width: 1024.
	pager asBrickedMorph openInWindow.
	
	pager inspect.