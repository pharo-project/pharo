tests
testDynamicBuild

	| model widget1 widget2 widget3 |
	model := TestingComposablePresenter new.
 	model openWithSpec close.
	widget1 := model list widget.
	
	model openWithSpec close.
	widget2 := model list widget.
	
	self deny: widget1 == widget2.
	
	model list needRebuild: false.
	model needRebuild: false.
	model openWithSpec close.
	widget3 := model list widget.
	
	self assert: widget2 == widget3.
	self assert: model needRebuild