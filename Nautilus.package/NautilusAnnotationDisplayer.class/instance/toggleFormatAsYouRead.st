submorphs-accessing
toggleFormatAsYouRead
	| formatter |
	formatter := RBProgramNode formatterClass new.
	formatter class formatAsYouReadPolicy
		ifTrue: [ 
			formatter class formatAsYouReadPolicy: false. 
			self owner model canDiscardEdits
				ifTrue: [ self owner model cancelEdits ] ]
		ifFalse: [ 
			formatter class formatAsYouReadPolicy: true.
			self owner model
				formatSourceCodeInView;
				clearUserEdits ].
	self updateContents.
	self paragraphProvider changed