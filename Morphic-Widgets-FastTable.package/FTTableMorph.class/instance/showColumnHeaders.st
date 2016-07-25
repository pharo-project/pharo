accessing
showColumnHeaders
	"Indicates table will show column headers. 
	 See #hideColumnHeaders"
	showColumnHeaders ifTrue: [ ^ self ].
	showColumnHeaders := true.
	self refresh