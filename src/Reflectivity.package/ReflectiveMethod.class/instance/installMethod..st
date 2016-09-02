invalidate
installMethod: aMethod
	| selector |
	selector := aMethod selector.
	"add to method dictionary."
	class methodDict at: selector put: aMethod.