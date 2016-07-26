initialize-release
initialize 
	super initialize.
	presentationTransformation := [ :aComposite :each | aComposite custom: each ]