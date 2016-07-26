initialization
adapt: aModel
	
	super adapt: aModel.
	aModel whenBuiltDo: [ :w | w widget color: Smalltalk ui theme backgroundColor ]