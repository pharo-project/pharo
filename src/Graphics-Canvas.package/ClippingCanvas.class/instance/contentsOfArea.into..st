accessing
contentsOfArea: aRectangle into: aForm
	self flag: #hack.    "ignore the clipping specification for this command.  This is purely so that CachingCanvas will work properly when clipped.  There *has* to be a clean way to do this...."
	
	^canvas contentsOfArea: aRectangle into: aForm