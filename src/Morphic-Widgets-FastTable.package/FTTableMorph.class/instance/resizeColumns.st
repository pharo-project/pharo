private
resizeColumns
	"Resize columns just has sense if there is more than one column, otherwise it will be resized 
	 to row size, so I can safely skip the resize part if I have just one (and then I avoid 
	 redundancy)"
	self columns size > 1 ifFalse: [ ^ self ].
	self columns last 
		width: (self width - (self columns allButLast sum: [ :each | each width ifNil: [0]]))