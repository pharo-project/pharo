initialization
initialize
	super initialize.
	
	critiques := #().
	
	self
		layoutPolicy: TableLayout new;
		listDirection: #leftToRight;
		cellPositioning: #center;
		hResizing: #shrinkWrap;
		vResizing: #rigid;
		height: 51; "because it fits 3 row list without showing scrollbars"
		cellInset: 3;
		color: Color transparent.
	
	self initializeList.
	self initializeProgress.
	self initializeUpdateTask.
	
	showPrivacyMorph := self class wasOpenedBefore not.
	
	showPrivacyMorph
		ifTrue: [ 
			self class wasOpenedBefore: true.
			GLMAsyncTask new
				delay: 10000;
				onCompleted: [ showPrivacyMorph := false ];
				execute.
			self addMorphBack: self privacyMorph ];
		ifFalse: [
			self addMorphBack: listMorph ].
	
	self subscribe