displaying
installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor
	"Install the receiver on the given DisplayContext (either BitBlt or Canvas) for further drawing operations."
	^self subclassResponsibility