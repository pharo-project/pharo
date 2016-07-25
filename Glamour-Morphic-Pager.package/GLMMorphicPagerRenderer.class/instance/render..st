rendering
render: aBrowser
	|pagerModel|
	pagerModel := GLMPagerModel new
		isDependenciesShown: false;
		defaultNumberOfVisiblePages: aBrowser fixedSizePanes;
		minimumPageNumberForScrollbar: aBrowser minimumPageNumberForScrollbar.
	
	aBrowser scrollButtonIconRenderBlock ifNotNil: [
		"result value of the block is raw icon of the Form class"
		pagerModel scrollButtonIconRenderBlock: [ :index | aBrowser scrollButtonIconRenderBlock cull: ((aBrowser panes at: index) port: #entity) value ]
	].
	
	container := (GLMPagerBrick new
		pagerModel: pagerModel;
		yourself) asBrickedMorph.

	"container maxPanes: aBrowser fixedSizePanes."
	self registerAnnouncementsFor: aBrowser.  
	aBrowser panes
		do: [ :each | container brickRoot pushPane: (self renderObject: each) ].
	^ container